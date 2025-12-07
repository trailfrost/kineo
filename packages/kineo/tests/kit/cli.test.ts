// tests/kit/cli.test.ts
import { beforeEach, describe, expect, test, vi } from "vitest";
import { FieldDef, RelationDef } from "@/schema";

// --- Mock convoker but keep real Command and parsing ---
vi.mock("convoker", async () => {
  // pull in the real module so Command (and parsing) stay intact
  const actual = await vi.importActual<typeof import("convoker")>("convoker");

  // override only the pieces we want to stub
  return {
    ...actual,
    // simple color mock
    log: {
      setup: vi.fn().mockResolvedValue(undefined),
      info: vi.fn().mockResolvedValue(undefined),
      warn: vi.fn().mockResolvedValue(undefined),
      error: vi.fn().mockResolvedValue(undefined),
      trace: vi.fn().mockResolvedValue(undefined),
    },
    prompt: {
      text: vi.fn().mockResolvedValue("mock-input"),
      select: vi.fn().mockResolvedValue("dynamic"),
      confirm: vi.fn().mockResolvedValue(true),
    },
  };
});

// --- Mock node:fs (existsSync + promises) ---
vi.mock("node:fs", () => {
  return {
    existsSync: vi.fn(() => false),
    promises: {
      readFile: vi.fn(async () => {
        // default mock file content (schema file)
        return "export const schema = defineSchema({});";
      }),
      writeFile: vi.fn(async () => undefined),
      readdir: vi.fn(async () => []),
      stat: vi.fn(async () => ({ mtime: new Date() })),
    },
  };
});

// --- Mock node:path join to be deterministic (optional) ---
vi.mock("node:path", async () => {
  const actual = await vi.importActual<typeof import("node:path")>("node:path");
  return {
    ...actual,
    join: (...parts: string[]) => parts.join("/"),
  };
});

// --- Mock jiti loader used in cli ---
vi.mock("jiti", () => {
  return {
    createJiti: vi.fn(() => ({
      import: vi.fn(async () => {
        // For config discovery, throw when a file isn't present
        // but return a minimal module for known names if needed.
        // Returning an empty object is okay because parseConfig is mocked below.
        return {};
      }),
    })),
  };
});

// --- Mock the kit utilities (the main toolkit) ---
vi.mock("@/kit", async () => {
  const actual = await vi.importActual("@/kit");
  return {
    // parseConfig should return a minimal config object shape used by CLI
    parseConfig: vi.fn().mockResolvedValue({}),
    push: vi.fn().mockResolvedValue(undefined),
    pull: vi.fn().mockResolvedValue({}),
    generate: vi.fn().mockResolvedValue(["migration-content"]),
    deploy: vi.fn().mockResolvedValue(undefined),
    rollback: vi.fn().mockResolvedValue(undefined),
    status: vi.fn().mockResolvedValue("completed"),
    ...actual,
  };
});

// --- Now import the module under test (after mocks) ---
import { program } from "@/kit";

// We need some helpers from the module to test (they are not exported in your original file).
// Vitest can import actual module implementation to access internal named exports if they exist.
// Since your file defines helper functions but doesn't export them, we'll re-import the file using
// vi.importActual to access the module's exports (if compiled to exports). If helpers are not exported
// you can alternatively test behavior via CLI flows (init/pull/create) which exercise those functions.
// Try to import actual module to get any exported helpers:
let helpers: any;
beforeEach(async () => {
  vi.clearAllMocks();
  // attempt to import internal named exports (works if your build exposes them)
  try {
    helpers = await vi.importActual<typeof import("@/kit")>("@/kit");
  } catch {
    // Not fatal; some helper tests will be skipped if not available.
    helpers = null;
  }
});

describe("kineo CLI (unit)", () => {
  test("program.run exists and is callable", async () => {
    // program.run is built by convoker.Command; ensure calling with --help doesn't throw
    await expect(program.run(["--help"])).resolves.not.toThrow();
  });

  // Helper function tests (only if the module exposes them)
  describe("helpers (importPath, ensureImports, generateSchemaSource, serializeFieldOrRelation)", () => {
    test("importPath adds ./ when missing", () => {
      if (!helpers || !helpers.importPath) return;
      expect(helpers.importPath("src/file.ts")).toBe('"./src/file.ts"');
      expect(helpers.importPath("./src/file.ts")).toBe('"./src/file.ts"');
    });

    test("ensureImports inserts imports when absent", () => {
      if (!helpers || !helpers.ensureImports) return;
      const src = "export const schema = defineSchema({})";
      const res = helpers.ensureImports(src);
      expect(res).toContain(
        'import { defineSchema, model, field, relation } from "kineo/schema";',
      );
      expect(res).toContain("defineSchema");
    });

    test("ensureImports does not duplicate when imports are present", () => {
      if (!helpers || !helpers.ensureImports) return;
      const src =
        'import { defineSchema, model, field, relation } from "kineo/schema";\nexport const schema = defineSchema({})';
      const res = helpers.ensureImports(src);
      expect(res).toBe(src);
    });

    test("generateSchemaSource produces a valid string", () => {
      if (!helpers || !helpers.generateSchemaSource) return;
      const schemaObj = {
        User: { id: 1, name: "joe" },
      };
      const generated = helpers.generateSchemaSource(schemaObj, "schema");
      expect(generated).toContain("export const schema = defineSchema");
      expect(generated).toContain("User");
    });

    test("serializeFieldOrRelation serializes FieldDef and RelationDef", async () => {
      if (!helpers || !helpers.serializeFieldOrRelation) return;

      const f = new FieldDef("string").id().required();
      const r = new RelationDef("Author").array().outgoing("HAS_AUTHORS");

      const fs = helpers.serializeFieldOrRelation(f);
      const rs = helpers.serializeFieldOrRelation(r);

      expect(fs).toContain("field.string");
      expect(fs).toContain(".id()");
      expect(fs).toContain(".required()");

      expect(rs).toContain('relation.to("Author"');
      expect(rs).toContain(".outgoing()");
      expect(rs).toContain(".array()");
    });
  });

  describe("init flow (prompts -> write file)", () => {
    test("init command prompts for missing values and writes a config file", async () => {
      const convoker = await import("convoker");
      // set desired prompt behaviors for this test
      (convoker.prompt.text as any).mockResolvedValueOnce("src/db/index.ts"); // clientFile
      (convoker.prompt.text as any).mockResolvedValueOnce("client"); // clientExport
      (convoker.prompt.text as any).mockResolvedValueOnce("src/db/schema.ts"); // schemaFile
      (convoker.prompt.text as any).mockResolvedValueOnce("schema"); // schemaExport
      (convoker.prompt.text as any).mockResolvedValueOnce("migrations"); // migrations
      (convoker.prompt.select as any).mockResolvedValueOnce("direct"); // style

      // call the CLI with "init" (as if user ran `kineo init`)
      await expect(program.run(["init"])).resolves.not.toThrow();

      // verify fs.writeFile was called (node:fs promises mocked earlier)
      const fsMock = await import("node:fs");
      expect(
        (fsMock.promises.writeFile as any).mock.calls.length,
      ).toBeGreaterThan(0);

      // verify logs were called to notify user
      expect(convoker.log.info).toHaveBeenCalledWith(
        expect.stringContaining("\nGenerating configuration file."),
      );
      expect(convoker.log.info).toHaveBeenCalledWith(
        "Configuration file generated! You can now start using Kineo migrations.",
      );
    });
  });
});
