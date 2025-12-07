import { describe, test, expect } from "vitest";
import { compile } from "@/compiler/cypher";
import * as IR from "@/ir";

describe("compile()", () => {
  test("throws on unsupported statement type", () => {
    const ir = { statements: [{ type: "Unknown" }] as any };
    expect(() => compile(ir as any)).toThrow(/Unsupported statement type/);
  });

  test("compiles a Find statement with where, orderBy, skip, take, and include", () => {
    const ir = {
      statements: [
        {
          type: IR.StatementType.Find,
          model: "User",
          alias: "u",
          where: { age: { gt: 18 }, OR: [{ name: "Alice" }, { name: "Bob" }] },
          orderBy: [{ name: ["asc", "asc"] }],
          skip: 5,
          take: 10,
          include: {
            posts: {
              where: { title: { contains: "Neo4j" } },
              include: {
                comments: {},
              },
            },
          },
        },
      ],
    } as any;

    const { command, params } = compile(ir);

    expect(command).toContain("MATCH (u:User)");
    expect(command).toContain("WHERE");
    expect(command).toContain("ORDER BY u.name ASC");
    expect(command).toContain("SKIP 5");
    expect(command).toContain("LIMIT 10");
    expect(command).toContain("OPTIONAL MATCH (u)-[:POSTS]->(u_posts:posts)");
    expect(command).toContain("OPTIONAL MATCH (u_posts)-[:COMMENTS]->");
    expect(params).toMatchObject({
      age_1: 18,
      name_2: "Alice",
      name_3: "Bob",
    });
  });

  test("compiles a Count statement", () => {
    const ir = {
      statements: [
        {
          type: IR.StatementType.Count,
          model: "User",
          alias: "u",
          where: { active: true },
        },
      ],
    } as any;

    const { command, params } = compile(ir);
    expect(command).toContain("MATCH (u:User)");
    expect(command).toContain("RETURN count(u) AS count");
    expect(Object.keys(params)).toContain("active_1");
  });

  test("compiles a Create statement with props and select", () => {
    const ir = {
      statements: [
        {
          type: IR.StatementType.Create,
          model: "User",
          alias: "u",
          data: { name: "Alice", age: 30 },
          select: { name: true },
        },
      ],
    } as any;

    const { command, params } = compile(ir);
    expect(command).toContain("CREATE (u:User");
    expect(command).toContain("RETURN u.name AS name");
    expect(params).toMatchObject({
      create_name_1: "Alice",
      create_age_2: 30,
    });
  });

  test("compiles an Upsert with both create and update data", () => {
    const ir = {
      statements: [
        {
          type: IR.StatementType.Upsert,
          model: "User",
          alias: "u",
          where: { email: "a@b.com" },
          data: {
            create: { name: "Alice" },
            update: { age: 31 },
          },
        },
      ],
    } as any;

    const { command, params } = compile(ir);
    expect(command).toContain("MERGE (u:User");
    expect(command).toContain("ON CREATE SET");
    expect(command).toContain("ON MATCH SET");
    expect(command).toContain("RETURN properties(u) AS u");
    expect(Object.keys(params)).toEqual(
      expect.arrayContaining([
        "merge_email_1",
        "oncreate_name_2",
        "onmatch_age_3",
      ]),
    );
  });

  test("compiles an Upsert fallback to simple create when no where keys", () => {
    const ir = {
      statements: [
        {
          type: IR.StatementType.Upsert,
          model: "User",
          alias: "u",
          where: {},
          data: { create: { name: "Alice" } },
        },
      ],
    } as any;

    const { command } = compile(ir);
    expect(command).toContain("CREATE (u:User");
    expect(command).toContain("RETURN properties(u) AS u");
  });

  test("compiles a Delete statement", () => {
    const ir = {
      statements: [
        {
          type: IR.StatementType.Delete,
          model: "User",
          alias: "u",
          where: { id: 123 },
        },
      ],
    } as any;

    const { command, params } = compile(ir);
    expect(command).toContain("DELETE u");
    expect(params).toMatchObject({ id_1: 123 });
  });

  test("compiles a ConnectQuery statement (OUT direction)", () => {
    const ir = {
      statements: [
        {
          type: IR.StatementType.ConnectQuery,
          model: "User",
          relation: "FRIENDS_WITH",
          from: { id: 1 },
          to: { id: 2 },
          properties: { since: 2024 },
          direction: "OUT",
        },
      ],
    } as any;

    const { command, params } = compile(ir);
    expect(command).toContain("MERGE (a)-[r:FRIENDS_WITH");
    expect(command).toContain("RETURN properties(r) AS relation");
    expect(params).toMatchObject({
      rel_since_1: 2024,
      id_2: 1,
      id_3: 2,
    });
  });

  test("compiles a ConnectQuery with IN and BOTH directions", () => {
    const inIR = {
      statements: [
        {
          type: IR.StatementType.ConnectQuery,
          model: "User",
          relation: "KNOWS",
          from: { id: 1 },
          to: { id: 2 },
          direction: "IN",
        },
      ],
    } as any;
    const bothIR = {
      statements: [
        {
          type: IR.StatementType.ConnectQuery,
          model: "User",
          relation: "KNOWS",
          from: { id: 1 },
          to: { id: 2 },
          direction: "BOTH",
        },
      ],
    } as any;

    expect(compile(inIR).command).toContain("<-[r:KNOWS");
    expect(compile(bothIR).command).toContain("-[r:KNOWS");
  });

  test("compiles a RelationQuery statement with min/max/limit/direction", () => {
    const ir = {
      statements: [
        {
          type: IR.StatementType.RelationQuery,
          model: "User",
          from: { id: 1 },
          to: { id: 2 },
          direction: "IN",
          minDepth: 1,
          maxDepth: 3,
          limit: 2,
        },
      ],
    } as any;

    const { command } = compile(ir);
    expect(command).toContain("MATCH (a:User)");
    expect(command).toContain("<-[:*1..3]->");
    expect(command).toContain("RETURN p");
    expect(command).toContain("LIMIT 2");
  });
});
