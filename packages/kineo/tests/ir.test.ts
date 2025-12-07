import { describe, test, expect } from "vitest";
import {
  StatementType,
  compileFindStatement,
  compileCountStatement,
  compileCreateStatement,
  compileUpsertStatement,
  compileDeleteStatement,
  compileConnectQueryStatement,
  compileRelationQueryStatement,
  makeIR,
  compileToIR,
} from "@/ir";

// Mock model types — just need to shape opts as expected
const baseOpts = {
  where: { id: 1 },
  select: { id: true },
  include: { posts: true },
  orderBy: [{ id: "asc" as const }],
  distinct: ["id"],
  skip: 5,
  take: 10,
};

describe("compileFindStatement", () => {
  test("creates a valid FindStatement", () => {
    const stmt = compileFindStatement("User", baseOpts);
    expect(stmt).toEqual({
      type: StatementType.Find,
      model: "User",
      where: { id: 1 },
      select: { id: true },
      include: { posts: true },
      orderBy: [{ id: "asc" }],
      distinct: ["id"],
      skip: 5,
      take: 10,
    });
  });

  test("handles undefined optional fields", () => {
    const stmt = compileFindStatement("User", {});
    expect(stmt.type).toBe(StatementType.Find);
    expect(stmt.model).toBe("User");
  });
});

describe("compileCountStatement", () => {
  test("creates a valid CountStatement", () => {
    const stmt = compileCountStatement("Post", { where: { published: true } });
    expect(stmt).toEqual({
      type: StatementType.Count,
      model: "Post",
      where: { published: true },
    });
  });
});

describe("compileCreateStatement", () => {
  test("creates a valid CreateStatement", () => {
    const stmt = compileCreateStatement("User", {
      data: { name: "Alice" },
      select: { id: true },
      include: { posts: true },
    });
    expect(stmt).toEqual({
      type: StatementType.Create,
      model: "User",
      data: { name: "Alice" },
      select: { id: true },
      include: { posts: true },
    });
  });
});

describe("compileUpsertStatement", () => {
  test("creates a valid UpsertStatement", () => {
    const stmt = compileUpsertStatement("User", {
      where: { id: 1 },
      create: { name: "Alice" },
      update: { name: "Bob" },
      select: { id: true },
      include: { posts: true },
    });
    expect(stmt).toEqual({
      type: StatementType.Upsert,
      model: "User",
      where: { id: 1 },
      data: {
        create: { name: "Alice" },
        update: { name: "Bob" },
      },
      select: { id: true },
      include: { posts: true },
    });
  });
});

describe("compileDeleteStatement", () => {
  test("creates a valid DeleteStatement", () => {
    const stmt = compileDeleteStatement("User", { where: { id: 2 } });
    expect(stmt).toEqual({
      type: StatementType.Delete,
      model: "User",
      where: { id: 2 },
    });
  });
});

describe("compileConnectQueryStatement", () => {
  test("creates a valid ConnectQueryStatement", () => {
    const stmt = compileConnectQueryStatement("User", {
      from: { where: { id: 1 } },
      to: { where: { id: 2 } },
      relation: "FRIEND_OF",
      direction: "outgoing",
      properties: { since: 2020 },
    });
    expect(stmt).toEqual({
      type: StatementType.ConnectQuery,
      model: "User",
      from: { id: 1 },
      to: { id: 2 },
      relation: "FRIEND_OF",
      direction: "outgoing",
      properties: { since: 2020 },
    });
  });
});

describe("compileRelationQueryStatement", () => {
  test("creates a valid RelationQueryStatement", () => {
    const stmt = compileRelationQueryStatement("User", {
      from: { where: { id: 1 } },
      to: { where: { id: 3 } },
      maxDepth: 5,
      minDepth: 1,
      direction: "outgoing",
      limit: 10,
    });
    expect(stmt).toEqual({
      type: StatementType.RelationQuery,
      model: "User",
      from: { id: 1 },
      to: { id: 3 },
      maxDepth: 5,
      minDepth: 1,
      direction: "outgoing",
      limit: 10,
    });
  });
});

describe("makeIR", () => {
  test("wraps statements in IR", () => {
    const stmt = { type: StatementType.Find, model: "User" } as any;
    const ir = makeIR(stmt);
    expect(ir).toEqual({ statements: [stmt] });
  });
});

describe("compileToIR", () => {
  test("handles findFirst/findMany", () => {
    const ir = compileToIR("User", "findFirst", baseOpts);
    expect(ir.statements[0].type).toBe(StatementType.Find);
  });

  test("handles count", () => {
    const ir = compileToIR("Post", "count", { where: { active: true } });
    expect(ir.statements[0].type).toBe(StatementType.Count);
  });

  test("handles create", () => {
    const ir = compileToIR("User", "create", { data: { name: "A" } });
    expect(ir.statements[0].type).toBe(StatementType.Create);
  });

  test("handles upsert", () => {
    const ir = compileToIR("User", "upsert", {
      where: { id: 1 },
      create: { name: "A" },
      update: { name: "B" },
    });
    expect(ir.statements[0].type).toBe(StatementType.Upsert);
  });

  test("handles delete", () => {
    const ir = compileToIR("User", "delete", { where: { id: 3 } });
    expect(ir.statements[0].type).toBe(StatementType.Delete);
  });

  test("handles connect", () => {
    const ir = compileToIR("User", "connect", {
      from: { where: { id: 1 } },
      to: { where: { id: 2 } },
      relation: "FRIEND_OF",
    });
    expect(ir.statements[0].type).toBe(StatementType.ConnectQuery);
  });

  test("handles findPath", () => {
    const ir = compileToIR("User", "findPath", {
      from: { where: { id: 1 } },
      to: { where: { id: 2 } },
    });
    expect(ir.statements[0].type).toBe(StatementType.RelationQuery);
  });

  test("throws on unknown operation", () => {
    expect(() => compileToIR("User", "invalidOp", {})).toThrowError(
      /Unknown operation type: invalidOp/,
    );
  });
});
