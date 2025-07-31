import { describe, test, expect } from "vitest";
import compile from "../src/compiler";
import type { IR } from "../src/ir";

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
    expect(result.cypher).toMatch(/CREATE \(u:User \$props0\)/);
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
    expect(result.cypher).toContain("MATCH (u:User)");
    expect(result.cypher).toContain("WHERE u.age >= $u_age_0_0");
    expect(result.cypher).toContain("RETURN u.name, u.age");
    expect(result.params).toEqual({ u_age_0_0: 18 });
  });

  test("compiles MERGE wtesth ON CREATE and ON MATCH", () => {
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
    expect(result.cypher).toContain("MERGE (u:User { id: $u_id_0_0 })");
    expect(result.cypher).toContain(
      "ON MATCH SET u.lastLogin = $u_update_lastLogin_0_0"
    );
    expect(result.cypher).toContain(
      "ON CREATE SET u.createdAt = $u_create_createdAt_0_0"
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
    expect(result.cypher).toContain("MATCH (u:User)");
    expect(result.cypher).toContain("MATCH (p:Post)");
    expect(result.cypher).toContain("CREATE (u)-[:WROTE]->(p)");
    expect(result.params).toEqual({
      u_from_id_0_0: "user1",
      p_to_id_0_0: "post1",
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
          where: { ttestle: "First Post" },
        },
      ],
    };

    const result = compile(ir);
    expect(result.cypher).toContain("MATCH (u:User)");
    expect(result.cypher).toContain("MATCH (u)-[:WROTE]->(p:Post)");
    expect(result.cypher).toContain("WHERE p.ttestle = $p_ttestle_0_0");
    expect(result.params).toEqual({
      u_from_id_0_0: "user1",
      p_ttestle_0_0: "First Post",
    });
  });
});
