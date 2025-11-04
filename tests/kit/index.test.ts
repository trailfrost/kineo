import { describe, test, expect, vi } from "vitest";
import { push, pull, generate, deploy, status, rollback, getDiff } from "@/kit";
import { KineoKitError, KineoKitErrorKind } from "@/error";
import { defineModel, defineSchema, field } from "@/schema";
import type { Adapter } from "@/adapter";

// A minimal fake adapter
function createFakeAdapter(
  overrides: Partial<Adapter<any, any>> = {}
): Adapter<any, any> {
  return {
    Model: class FakeModel {},
    compile: vi.fn(),
    exec: vi.fn(),
    close: vi.fn(),
    ...overrides,
  };
}

const simpleSchema = defineSchema({
  User: defineModel({
    id: field.int().required(),
  }),
});

describe("push()", () => {
  test("throws if adapter lacks pull or push", async () => {
    const adapter = createFakeAdapter({});
    await expect(push(adapter, simpleSchema)).rejects.toThrowError(
      KineoKitError
    );
  });

  test("throws on breaking schema diff", async () => {
    const adapter = createFakeAdapter({
      pull: vi.fn().mockResolvedValue({
        User: defineModel({
          id: field.int().id(),
          name: field.string(),
        }),
      }),
      push: vi.fn(),
    });

    const newSchema = defineSchema({
      User: defineModel({
        id: field.int().id(),
      }),
    });

    await expect(push(adapter, newSchema)).rejects.toMatchObject({
      kind: KineoKitErrorKind.BreakingSchemaChange,
    });
  });

  test("calls push when no breaking changes", async () => {
    const adapter = createFakeAdapter({
      pull: vi.fn().mockResolvedValue(simpleSchema),
      push: vi.fn(),
    });

    await push(adapter, simpleSchema);
    expect(adapter.push).toHaveBeenCalledWith(simpleSchema);
  });

  test("skips diff check when force = true", async () => {
    const adapter = createFakeAdapter({
      pull: vi.fn().mockRejectedValue(new Error("should not be called")),
      push: vi.fn(),
    });

    await push(adapter, simpleSchema, true);
    expect(adapter.push).toHaveBeenCalledWith(simpleSchema);
  });
});

describe("getDiff()", () => {
  test("detects added and removed models", () => {
    const prev = { User: {} };
    const cur = { Account: {} };
    const diff = getDiff(prev, cur);
    expect(diff.breaking).toContain('Model "User" was removed');
    expect(diff.nonBreaking).toContain('Model "Account" was added');
  });

  test("detects added and removed fields", () => {
    const prev = defineSchema({ User: defineModel({ id: field.int() }) });
    const cur = defineSchema({ User: defineModel({ name: field.string() }) });
    const diff = getDiff(prev, cur);
    expect(diff.breaking[0]).toMatch(/id/);
    expect(diff.nonBreaking[0]).toMatch(/name/);
  });
});

describe("pull()", () => {
  test("throws if adapter lacks pull", async () => {
    const adapter = createFakeAdapter({});
    await expect(pull(adapter)).rejects.toThrowError(KineoKitError);
  });

  test("returns schema if adapter.pull exists", async () => {
    const adapter = createFakeAdapter({
      pull: vi.fn().mockResolvedValue(simpleSchema),
    });

    const result = await pull(adapter);
    expect(result).toBe(simpleSchema);
    expect(adapter.pull).toHaveBeenCalled();
  });
});

describe("generate()", () => {
  test("throws if adapter lacks generate", async () => {
    const adapter = createFakeAdapter({});
    await expect(generate(adapter, simpleSchema, simpleSchema)).rejects.toThrow(
      KineoKitError
    );
  });

  test("calls adapter.generate()", async () => {
    const adapter = createFakeAdapter({
      generate: vi.fn().mockResolvedValue(["migration.sql"]),
    });
    const result = await generate(adapter, simpleSchema, simpleSchema);
    expect(adapter.generate).toHaveBeenCalled();
    expect(result).toEqual(["migration.sql"]);
  });
});

describe("deploy()", () => {
  test("throws if adapter lacks deploy", async () => {
    const adapter = createFakeAdapter({});
    await expect(deploy(adapter, "m1")).rejects.toThrow(KineoKitError);
  });

  test("calls deploy with hash", async () => {
    const adapter = createFakeAdapter({
      deploy: vi.fn(),
    });

    // crypto.hash doesn't exist — you can mock it to test call shape
    vi.mock("node:crypto", () => ({
      default: { hash: vi.fn().mockReturnValue("hash123") },
    }));

    await deploy(adapter, "migration1");
    expect(adapter.deploy).toHaveBeenCalled();
  });
});

describe("status()", () => {
  test("throws if adapter lacks status", async () => {
    const adapter = createFakeAdapter({});
    await expect(status(adapter, "m1")).rejects.toThrow(KineoKitError);
  });

  test("calls status with hash", async () => {
    const adapter = createFakeAdapter({
      status: vi.fn().mockResolvedValue("completed"),
    });

    vi.mock("node:crypto", () => ({
      default: { hash: vi.fn().mockReturnValue("hash123") },
    }));

    const result = await status(adapter, "migration1");
    expect(adapter.status).toHaveBeenCalled();
    expect(result).toBe("completed");
  });
});

describe("rollback()", () => {
  test("throws if adapter lacks rollback", async () => {
    const adapter = createFakeAdapter({});
    await expect(rollback(adapter, "m1")).rejects.toThrow(KineoKitError);
  });

  test("calls rollback with hash", async () => {
    const adapter = createFakeAdapter({
      rollback: vi.fn(),
    });

    vi.mock("node:crypto", () => ({
      default: { hash: vi.fn().mockReturnValue("hash123") },
    }));

    await rollback(adapter, "migration1");
    expect(adapter.rollback).toHaveBeenCalled();
  });
});
