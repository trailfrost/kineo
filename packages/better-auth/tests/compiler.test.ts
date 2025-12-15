import { describe, expect, test, vi } from "vitest";
import type { CleanedWhere } from "better-auth/adapters";
import { compile } from "@/compiler";

/**
 * Mock kineo/ir so we can assert how it's called
 */
vi.mock("kineo/ir", () => {
  return {
    makeIR: vi.fn((value) => ({ __ir: value })),
    compileFindStatement: vi.fn((model, args) => ({
      type: "find",
      model,
      args,
    })),
    compileCountStatement: vi.fn((model, args) => ({
      type: "count",
      model,
      args,
    })),
    compileCreateStatement: vi.fn((model, args) => ({
      type: "create",
      model,
      args,
    })),
    compileUpsertStatement: vi.fn((model, args) => ({
      type: "upsert",
      model,
      args,
    })),
    compileDeleteStatement: vi.fn((model, args) => ({
      type: "delete",
      model,
      args,
    })),
  };
});

import * as IR from "kineo/ir";

describe("compile", () => {
  test("compiles findMany with select, where, sort, limit, and offset", () => {
    const where: CleanedWhere[] = [
      { field: "email", operator: "eq", value: "a@test.com", connector: "AND" },
    ];

    const result = compile("findMany", {
      model: "User",
      where,
      select: ["id", "email"],
      limit: 10,
      offset: 5,
      sortBy: { field: "email", direction: "asc" },
    });

    expect(IR.compileFindStatement).toHaveBeenCalledWith("User", {
      where: { email: "a@test.com" },
      select: { id: true, email: true },
      include: undefined,
      orderBy: [{ email: "asc" }],
      skip: 5,
      take: 10,
    });

    expect(IR.makeIR).toHaveBeenCalled();
    expect(result).toEqual({
      __ir: expect.objectContaining({ type: "find" }),
    });
  });

  test("compiles multiple where clauses using AND", () => {
    const where: CleanedWhere[] = [
      { field: "age", operator: "gte", value: 18, connector: "AND" },
      { field: "age", operator: "lt", value: 65, connector: "AND" },
    ];

    compile("findMany", {
      model: "User",
      where,
    });

    expect(IR.compileFindStatement).toHaveBeenCalledWith("User", {
      where: {
        AND: [{ age: { gte: 18 } }, { age: { lt: 65 } }],
      },
      select: undefined,
      include: undefined,
      orderBy: undefined,
      skip: undefined,
      take: undefined,
    });
  });

  test("compiles joins into include", () => {
    compile("findMany", {
      model: "User",
      join: {
        posts: { on: { from: "posts", to: "comments" }, limit: 5 },
        comments: { on: { from: "comments", to: "posts" }, limit: 2 },
      },
    });

    expect(IR.compileFindStatement).toHaveBeenCalledWith("User", {
      where: undefined,
      select: undefined,
      include: {
        posts: { take: 5 },
        comments: { take: 2 },
      },
      orderBy: undefined,
      skip: undefined,
      take: undefined,
    });
  });

  test("compiles count", () => {
    compile("count", {
      model: "User",
      where: [
        { field: "active", connector: "AND", operator: "eq", value: true },
      ],
    });

    expect(IR.compileCountStatement).toHaveBeenCalledWith("User", {
      where: { active: true },
    });
  });

  test("compiles create", () => {
    compile("create", {
      model: "User",
      data: { email: "a@test.com" },
    });

    expect(IR.compileCreateStatement).toHaveBeenCalledWith("User", {
      data: { email: "a@test.com" },
    });
  });

  test("compiles update as upsert", () => {
    compile("update", {
      model: "User",
      where: [{ field: "id", connector: "AND", operator: "eq", value: "1" }],
      data: { email: "b@test.com" },
    });

    expect(IR.compileUpsertStatement).toHaveBeenCalledWith("User", {
      where: { id: "1" },
      create: { email: "b@test.com" },
      update: { email: "b@test.com" },
    });
  });

  test("compiles delete", () => {
    compile("delete", {
      model: "User",
      where: [{ field: "id", connector: "AND", operator: "eq", value: "1" }],
    });

    expect(IR.compileDeleteStatement).toHaveBeenCalledWith("User", {
      where: { id: "1" },
    });
  });

  test("throws on unsupported mode", () => {
    expect(() => compile("unknown", { model: "User" })).toThrow(
      "Unsupported adapter mode",
    );
  });

  test("throws on unsupported where operator", () => {
    expect(() =>
      compile("findMany", {
        model: "User",
        where: [
          // @ts-expect-error – intentional invalid operator
          { field: "id", operator: "between", value: [1, 2] },
        ],
      }),
    ).toThrow("Unsupported operator");
  });
});
