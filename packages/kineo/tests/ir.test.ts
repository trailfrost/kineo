import { test, expect, describe } from "vitest";
import {
  parseMatch,
  parseCreate,
  parseMerge,
  parseDelete,
  parseConnect,
  parseDisconnect,
  parseRelationQuery,
  parseWhereNode,
} from "../src/ir";
import {
  defineSchema,
  model,
  field,
  relation,
  type InferNode,
} from "../src/schema";

export const schema = defineSchema({
  User: model({
    name: field.string().id(),
    password: field.string().required(),
    posts: relation.to("Post").outgoing("posts").array(),
  }),

  Post: model({
    id: field.string().id(),
    title: field.string().required(),
    author: relation.to("User").incoming("posts"),
  }),
});

// Simplified types for test readability
type Schema = typeof schema;
type User = Schema["User"];
type Post = Schema["Post"];

describe("IR Parsers", () => {
  test("parseMatch with complex where", () => {
    const match = parseMatch<Schema, User>("User", "u", {
      where: {
        name: { equals: "Alice" },
        AND: [
          {
            password: { contains: "secret" },
          },
          {
            name: { not: { equals: "Bob" } },
          },
        ],
        OR: [{ name: { startsWith: "A" } }, { name: { endsWith: "Z" } }],
      },
      orderBy: { name: "asc" },
      limit: 10,
      skip: 5,
      include: { posts: true },
      select: { name: true },
    });

    expect(match).toEqual({
      type: "MATCH",
      label: "User",
      alias: "u",
      where: {
        conditions: [{ field: "name", operator: "EQUALS", value: "Alice" }],
        AND: [
          {
            conditions: [
              { field: "password", operator: "CONTAINS", value: "secret" },
            ],
          },
          {
            conditions: [{ field: "name", operator: "NOT", value: "Bob" }],
          },
        ],
        OR: [
          {
            conditions: [
              { field: "name", operator: "STARTS_WITH", value: "A" },
            ],
          },
          {
            conditions: [{ field: "name", operator: "ENDS_WITH", value: "Z" }],
          },
        ],
      },
      orderBy: { name: "asc" },
      limit: 10,
      skip: 5,
      include: ["posts"],
      select: ["name"],
    });
  });

  test("parseCreate", () => {
    const create = parseCreate<Schema, User>("User", "u", {
      data: { name: "Bob", password: "pass123" },
    });

    expect(create).toEqual({
      type: "CREATE",
      label: "User",
      alias: "u",
      data: { name: "Bob", password: "pass123" },
    });
  });

  test("parseMerge", () => {
    const merge = parseMerge<Schema, User>("User", "u", {
      where: { name: "Alice" },
      create: { name: "hello", password: "world" },
      update: { password: "newpw" },
    });

    expect(merge).toEqual({
      type: "MERGE",
      label: "User",
      alias: "u",
      where: { name: "Alice" },
      create: { name: "hello", password: "world" },
      update: { password: "newpw" },
    });
  });

  test("parseDelete", () => {
    const del = parseDelete<Schema, User>("User", "u", {
      where: { name: "Alice" },
    });

    expect(del).toEqual({
      type: "DELETE",
      label: "User",
      alias: "u",
      where: { name: "Alice" },
    });
  });

  test("parseConnect", () => {
    const connect = parseConnect<Schema, Post>(
      "Post",
      "p",
      {
        from: { id: "post1" },
        to: { name: "Alice" },
        relation: "author",
      },
      schema.Post,
    );

    expect(connect).toEqual({
      type: "CONNECT",
      label: "Post",
      alias: "p",
      from: {
        label: "Post",
        alias: "from",
        match: { id: "post1" },
      },
      to: {
        label: "User",
        alias: "to",
        match: { name: "Alice" },
      },
      relation: "author",
    });
  });

  test("parseDisconnect", () => {
    const disconnect = parseDisconnect<Schema, Post>(
      "Post",
      "p",
      {
        from: { id: "post1" },
        to: { name: "Alice" },
        relation: "author",
      },
      schema.Post,
    );

    expect(disconnect.type).toBe("DISCONNECT");
    expect(disconnect.from.match.id).toBe("post1");
    expect(disconnect.to.label).toBe("User");
  });

  test("parseRelationQuery", () => {
    const relationQuery = parseRelationQuery<Schema, User>(schema, "User", {
      from: { name: "Alice" },
      relation: "posts",
      where: { id: "post123" },
    });

    expect(relationQuery).toEqual({
      type: "RELATION_QUERY",
      label: "Post",
      alias: "n",
      from: {
        label: "User",
        alias: "from",
        match: { name: "Alice" },
      },
      relation: "posts",
      where: { id: "post123" },
    });
  });

  test("parseWhereNode basic", () => {
    const where = parseWhereNode({
      name: { equals: "Alice" },
    });

    expect(where).toEqual({
      conditions: [{ field: "name", operator: "EQUALS", value: "Alice" }],
    });
  });

  test("parseWhereNode with NOT object", () => {
    const where = parseWhereNode({
      NOT: {
        name: { equals: "Bob" },
      },
    });

    expect(where).toEqual({
      NOT: {
        conditions: [{ field: "name", operator: "EQUALS", value: "Bob" }],
      },
    });
  });

  test("parseWhereNode with AND/OR", () => {
    const where = parseWhereNode<InferNode<User, Schema>>({
      AND: [
        { name: { equals: "Alice" } },
        { password: { contains: "secret" } },
      ],
      OR: [{ name: { startsWith: "A" } }, { name: { endsWith: "Z" } }],
    });

    expect(where).toEqual({
      AND: [
        {
          conditions: [{ field: "name", operator: "EQUALS", value: "Alice" }],
        },
        {
          conditions: [
            { field: "password", operator: "CONTAINS", value: "secret" },
          ],
        },
      ],
      OR: [
        {
          conditions: [{ field: "name", operator: "STARTS_WITH", value: "A" }],
        },
        {
          conditions: [{ field: "name", operator: "ENDS_WITH", value: "Z" }],
        },
      ],
    });
  });
});
