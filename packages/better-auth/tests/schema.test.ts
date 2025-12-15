import { describe, test, expect, vi, beforeEach } from "vitest";
import fs from "fs/promises";
import { createSchema, replaceSchemaInPlace } from "@/schema";

vi.mock("fs/promises", () => ({
  default: {
    readFile: vi.fn(),
    writeFile: vi.fn(),
  },
}));

const mockedFs = vi.mocked(fs);

describe("replaceSchemaInPlace", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  test("replaces named exported schema in-place", async () => {
    const fileContents = `
import { defineSchema } from "x";

export const schema = defineSchema({
  foo: "bar",
});
`;

    mockedFs.readFile.mockResolvedValueOnce(fileContents);
    mockedFs.writeFile.mockResolvedValueOnce(undefined);

    await replaceSchemaInPlace("db.ts", `  users: model("User", {})`);

    expect(mockedFs.writeFile).toHaveBeenCalledTimes(1);

    const written = mockedFs.writeFile.mock.calls[0][1];

    expect(written).toContain(`export const schema = defineSchema({`);
    expect(written).toContain(`users: model("User"`);
    expect(written).not.toContain(`foo: "bar"`);
  });

  test("replaces default exported schema in-place", async () => {
    const fileContents = `
export default defineSchema({
  hello: "world",
});
`;

    mockedFs.readFile.mockResolvedValueOnce(fileContents);
    mockedFs.writeFile.mockResolvedValueOnce(undefined);

    await replaceSchemaInPlace("schema.ts", `  things: model("Thing", {})`);

    const written = mockedFs.writeFile.mock.calls[0][1];

    expect(
      (written as string).trim().startsWith("export default defineSchema"),
    ).toBe(true);
    expect(written).toContain(`things: model("Thing"`);
    expect(written).not.toContain(`hello: "world"`);
  });

  test("throws if no schema is found", async () => {
    mockedFs.readFile.mockResolvedValueOnce(`export const x = 1;`);

    expect(await replaceSchemaInPlace("db.ts", `  foo: "bar"`)).toBeUndefined();
  });
});

describe("createSchema", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  test("writes schema into existing file", async () => {
    mockedFs.readFile.mockResolvedValueOnce(`
import { defineSchema } from "x";

export const schema = defineSchema({
  existing: true,
});
`);

    mockedFs.writeFile.mockResolvedValueOnce(undefined);

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

    const result = await createSchema({
      tables,
      file: "db.ts",
    });

    expect(result).toEqual({
      path: "db.ts",
      code: "",
    });

    const written = mockedFs.writeFile.mock.calls[0][1];

    expect(written).toContain(`users: model("User"`);
    expect(written).toContain(`id: field.string().required()`);
    expect(written).toContain(`email: field.string().index()`);
    expect(written).not.toContain(`existing: true`);
  });

  test("preserves client and other code", async () => {
    mockedFs.readFile.mockResolvedValueOnce(`
export const client = Kineo({});

export const schema = defineSchema({
  old: true,
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

    await createSchema({
      tables,
      file: "db.ts",
    });

    const written = mockedFs.writeFile.mock.calls[0][1];

    expect(written).toContain(`export const client = Kineo({});`);
    expect(written).toContain(`posts: model("Post"`);
    expect(written).not.toContain(`old: true`);
  });
});
