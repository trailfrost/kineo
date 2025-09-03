import compile from "../../src/compilers/cypher";
import type { IR } from "../../src/ir";
import { describe, test, expect } from "vitest";

describe("Compiler", () => {
  test("compiles a simple CREATE", () => {
    const ir: IR = {
      statements: [
        {
          type: "CREATE",
          label: "User",
          alias: "u",
          data: { name: "Alice", age: 30 },
        },
      ],
    };

    const result = compile(ir);
    expect(result.command).toMatch(/CREATE \(u:User \$props0\)/);
    expect(result.params).toEqual({ props0: { name: "Alice", age: 30 } });
  });

  test("compiles a simple MATCH wtesth WHERE", () => {
    const ir: IR = {
      statements: [
        {
          type: "MATCH",
          label: "User",
          alias: "u",
          where: {
            conditions: [{ field: "age", operator: "GTE", value: 18 }],
          },
          select: ["u.name", "u.age"],
        },
      ],
    };

    const result = compile(ir);
    expect(result.command).toContain("MATCH (u:User)");
    expect(result.command).toContain("WHERE u.age >= $u_age_0_0");
    expect(result.command).toContain("RETURN u.name, u.age");
    expect(result.params).toEqual({ u_age_0_0: 18 });
  });

  test("compiles MERGE with ON CREATE and ON MATCH", () => {
    const ir: IR = {
      statements: [
        {
          type: "MERGE",
          label: "User",
          alias: "u",
          where: { id: "123" },
          create: { createdAt: "now" },
          update: { lastLogin: "today" },
        },
      ],
    };

    const result = compile(ir);
    expect(result.command).toContain("MERGE (u:User { id: $u_id_0_0 })");
    expect(result.command).toContain(
      "ON MATCH SET u.lastLogin = $u_update_lastLogin_0_0",
    );
    expect(result.command).toContain(
      "ON CREATE SET u.createdAt = $u_create_createdAt_0_0",
    );
    expect(result.params).toMatchObject({
      u_id_0_0: "123",
      u_update_lastLogin_0_0: "today",
      u_create_createdAt_0_0: "now",
    });
  });

  test("compiles CONNECT between two nodes", () => {
    const ir: IR = {
      statements: [
        {
          type: "CONNECT",
          label: "Post",
          alias: "p",
          relation: "WROTE",
          from: {
            label: "User",
            alias: "u",
            match: { id: "user1" },
          },
          to: {
            label: "Post",
            alias: "p",
            match: { id: "post1" },
          },
        },
      ],
    };

    const result = compile(ir);
    expect(result.command).toContain("MATCH (u:User)");
    expect(result.command).toContain("MATCH (p:Post)");
    expect(result.command).toContain("CREATE (u)-[:WROTE]->(p)");
    expect(result.params).toEqual({
      u_id_0_0: "user1",
      p_id_0_0: "post1",
    });
  });

  test("compiles RELATION_QUERY from one node", () => {
    const ir: IR = {
      statements: [
        {
          type: "RELATION_QUERY",
          label: "Post",
          alias: "p",
          relation: "WROTE",
          from: {
            label: "User",
            alias: "u",
            match: { id: "user1" },
          },
          where: { title: "First Post" },
        },
      ],
    };

    const result = compile(ir);
    expect(result.command).toContain("MATCH (u:User)");
    expect(result.command).toContain("MATCH (u)-[:WROTE]->(p:Post)");
    expect(result.command).toContain("WHERE p.title = $p_title_0_0");
    expect(result.params).toEqual({
      u_id_0_0: "user1",
      p_title_0_0: "First Post",
    });
  });
});
