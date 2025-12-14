import { describe, test, expect, vi, beforeEach } from "vitest";
import fs from "fs/promises";
import {
  createSchema,
  deleteClientAndSchema,
  type ClientAndSchema,
} from "@/schema";

vi.mock("fs/promises", () => ({
  default: {
    readFile: vi.fn(),
    writeFile: vi.fn(),
  },
}));

const mockedFs = vi.mocked(fs);

describe("deleteClientAndSchema", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  test("returns empty object when no file is provided", async () => {
    const result = await deleteClientAndSchema();

    expect(result).toEqual({});
    expect(mockedFs.readFile).not.toHaveBeenCalled();
    expect(mockedFs.writeFile).not.toHaveBeenCalled();
  });

  test("extracts named exported schema and client and rewrites file", async () => {
    const fileContents = `
import { defineSchema } from "x";

export const schema = defineSchema({
  foo: "bar",
});

export const client = Kineo({}, schema);
`;

    mockedFs.readFile.mockResolvedValueOnce(fileContents);

    const result = await deleteClientAndSchema("db.ts");

    expect(result).toEqual<ClientAndSchema>({
      schema: {
        default: false,
        contents: `{
  foo: "bar",
}`,
        exportName: "schema",
      },
      client: `export const client = Kineo({}, schema);`,
    });

    expect(mockedFs.writeFile).toHaveBeenCalledTimes(1);

    const written = mockedFs.writeFile.mock.calls[0][1];
    expect(written).not.toContain("export const schema");
    expect(written).not.toContain("export const client");
  });

  test("extracts default exported schema", async () => {
    const fileContents = `
export default defineSchema({
  hello: "world",
});
`;

    mockedFs.readFile.mockResolvedValueOnce(fileContents);

    const result = await deleteClientAndSchema("schema.ts");

    expect(result.schema).toEqual({
      default: true,
      contents: `{
  hello: "world",
}`,
      exportName: "default",
    });
  });
});

describe("createSchema", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  test("generates schema code without file input", async () => {
    const tables = {
      users: {
        modelName: "User",
        fields: {
          id: {
            type: "string",
            required: true,
          },
          email: {
            type: "string",
            index: true,
          },
        },
      },
    } as any;

    const result = await createSchema({ tables });

    expect(result.path).toBe("./src/db.ts");
    expect(result.append).toBe(true);

    expect(result.code).toContain(`export const schema = defineSchema`);
    expect(result.code).toContain(`users: model("User"`);
    expect(result.code).toContain(`id: field.string().required()`);
    expect(result.code).toContain(`email: field.string().index()`);
  });

  test("reuses existing schema and client from file", async () => {
    mockedFs.readFile.mockResolvedValueOnce(`
export const client = Kineo({});

export const schema = defineSchema({
  existing: true,
});
`);

    mockedFs.writeFile.mockResolvedValueOnce(undefined);

    const tables = {
      posts: {
        modelName: "Post",
        fields: {
          title: {
            type: "string",
            required: true,
          },
        },
      },
    } as any;

    const result = await createSchema({
      tables,
      file: "db.ts",
    });

    expect(result.code).toContain(`export const schema = defineSchema`);
    expect(result.code).toContain(`existing: true`);
    expect(result.code).toContain(`posts: model("Post"`);

    // client should be preserved at the top
    expect(result.code.startsWith(`export const client = Kineo({});`)).toBe(
      true,
    );
  });

  test("emits default export when previous schema was default", async () => {
    mockedFs.readFile.mockResolvedValueOnce(`
export default defineSchema({
  old: true,
});
`);

    mockedFs.writeFile.mockResolvedValueOnce(undefined);

    const tables = {
      things: {
        modelName: "Thing",
        fields: {
          name: {
            type: "string",
          },
        },
      },
    } as any;

    const result = await createSchema({
      tables,
      file: "schema.ts",
    });

    expect(result.code.trim().startsWith("export default defineSchema")).toBe(
      true,
    );
    expect(result.code).toContain(`things: model("Thing"`);
  });
});
