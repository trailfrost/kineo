import { describe, test, expect, vi } from "vitest";
import compile from "@/compiler/sql";
import * as IR from "@/ir";

// A minimal mock dialect implementation for testing
const mockDialect = {
  identifier: vi.fn((name: string) => `"${name}"`),
  string: vi.fn((value: string) => `'${value}'`),
  array: vi.fn(
    (values: unknown[]) => `(${values.map((v) => `'${v}'`).join(", ")})`
  ),
  limitOffset: vi.fn((limit?: number, offset?: number) => {
    const clauses = [];
    if (limit !== undefined) clauses.push(`LIMIT ${limit}`);
    if (offset !== undefined) clauses.push(`OFFSET ${offset}`);
    return clauses.join(" ");
  }),
  boolean: vi.fn((value: boolean) => (value ? "TRUE" : "FALSE")),
  upsert: vi.fn((table: string, args: any) => {
    return `UPSERT INTO ${table} (${args.insertColumns.join(", ")}) VALUES (${args.insertValues.join(", ")}) ON CONFLICT (${args.conflictTarget.join(", ")}) DO UPDATE SET ${args.updateAssignments.join(", ")} ${args.returning}`;
  }),
  autoIncrement: vi.fn(() => "SERIAL"),
  now: vi.fn(() => "CURRENT_TIMESTAMP"),
  jsonExtract: vi.fn(
    (column: string, path: string) => `JSON_EXTRACT(${column}, '${path}')`
  ),
  type: vi.fn((type: string) => type.toUpperCase()),
  returning: vi.fn((columns?: string[]) =>
    columns?.length ? `RETURNING ${columns.join(", ")}` : ""
  ),
};

describe("SQL Compiler", () => {
  test("compiles a simple Find statement", () => {
    const ir: IR.IR = {
      statements: [
        {
          type: IR.StatementType.Find,
          model: "User",
          where: { id: 1 },
          select: { id: true, name: true },
        } as any,
      ],
    };

    const result = compile(ir, mockDialect);
    expect(result.command).toContain('SELECT "id", "name" FROM "User"');
    expect(result.command).toContain('WHERE "id" = 1');
    expect(result.params).toEqual({});
  });

  test("compiles a Find with orderBy and pagination", () => {
    const ir: IR.IR = {
      statements: [
        {
          type: IR.StatementType.Find,
          model: "Post",
          orderBy: [{ createdAt: "desc" }],
          skip: 5,
          take: 10,
        } as any,
      ],
    };

    const result = compile(ir, mockDialect);
    expect(result.command).toContain('SELECT * FROM "Post"');
    expect(result.command).toContain('ORDER BY "createdAt" DESC');
    expect(result.command).toContain("LIMIT 10 OFFSET 5");
  });

  test("compiles a Count statement with WHERE clause", () => {
    const ir: IR.IR = {
      statements: [
        {
          type: IR.StatementType.Count,
          model: "User",
          where: { active: true },
        } as any,
      ],
    };

    const result = compile(ir, mockDialect);
    expect(result.command).toBe(
      'SELECT COUNT(*) AS count FROM "User" WHERE "active" = TRUE'
    );
  });

  test("compiles a Create statement with data", () => {
    const ir: IR.IR = {
      statements: [
        {
          type: IR.StatementType.Create,
          model: "User",
          data: { name: "Alice", age: 30 },
          select: { id: true },
        } as any,
      ],
    };

    const result = compile(ir, mockDialect);
    expect(result.command).toBe(
      'INSERT INTO "User" ("name", "age") VALUES (\'Alice\', 30) RETURNING id'
    );
  });

  test("compiles a Create with empty data (DEFAULT VALUES)", () => {
    const ir: IR.IR = {
      statements: [
        {
          type: IR.StatementType.Create,
          model: "User",
          data: {},
        } as any,
      ],
    };

    const result = compile(ir, mockDialect);
    expect(result.command).toContain('INSERT INTO "User" DEFAULT VALUES');
  });

  test("compiles an Upsert statement", () => {
    const ir: IR.IR = {
      statements: [
        {
          type: IR.StatementType.Upsert,
          model: "User",
          where: { id: 1 },
          data: {
            create: { id: 1, name: "Alice" },
            update: { name: "Bob" },
          },
          select: { id: true },
        } as any,
      ],
    };

    const result = compile(ir, mockDialect);
    expect(result.command).toContain('UPSERT INTO "User"');
    expect(mockDialect.upsert).toHaveBeenCalled();
  });

  test("compiles a Delete statement with WHERE", () => {
    const ir: IR.IR = {
      statements: [
        {
          type: IR.StatementType.Delete,
          model: "User",
          where: { id: 42 },
        } as any,
      ],
    };

    const result = compile(ir, mockDialect);
    expect(result.command).toBe('DELETE FROM "User" WHERE "id" = 42');
  });

  test("throws on unsupported statement types", () => {
    const ir: IR.IR = {
      statements: [
        {
          type: IR.StatementType.ConnectQuery,
          model: "Graph",
          from: {},
          to: {},
          relation: "REL",
        } as any,
      ],
    };

    expect(() => compile(ir, mockDialect)).toThrow(/graph operations/);
  });

  test("supports JSON path extraction in where clause", () => {
    const ir: IR.IR = {
      statements: [
        {
          type: IR.StatementType.Find,
          model: "User",
          where: { "profile.name": "Alice" },
        } as any,
      ],
    };

    const result = compile(ir, mockDialect);
    expect(result.command).toContain("JSON_EXTRACT");
  });
});
