import { describe, test, expect } from "vitest";
import { db } from "./utils";

describe("Model", () => {
  test("createOne: creates a user", async () => {
    const result = await db.users.createOne({
      data: {
        name: "alice",
        password: "secure123",
      },
    });

    expect(result).toEqual({
      name: "alice",
      password: "secure123",
      posts: [],
    });
  });

  test("matchOne: match user by name with nested where", async () => {
    const user = await db.users.findOne({
      where: {
        name: {
          startsWith: "a",
          not: {
            endsWith: "z",
          },
        },
        AND: [
          {
            password: {
              contains: "secure",
            },
          },
        ],
      },
      include: {
        posts: true,
      },
    });

    expect(user?.name).toBe("alice");
    expect(user?.posts).toBeDefined();
  });

  test("mergeOne: merge user (create or update)", async () => {
    const merged = await db.users.upsertOne({
      where: { name: "alice" },
      create: {
        name: "alice",
        password: "secure123",
      },
      update: {
        password: "updated!",
      },
    });

    expect(merged?.name).toBe("alice");
    expect(merged?.password).toBe("updated!");
  });

  test("connect: connect user to post", async () => {
    await db.posts.createOne({
      data: {
        id: "post-1",
        title: "Hello World",
      },
    });

    const result = await db.users.connect({
      from: { name: "alice" },
      to: { id: "post-1" },
      relation: "posts",
    });

    expect(result).toBeDefined();
  });

  test("getRelations: get posts for user", async () => {
    const posts = await db.users.getRelations({
      from: { name: "alice" },
      relation: "posts",
      where: {
        title: "Hello World",
      },
    });

    expect(posts.length).toBeGreaterThanOrEqual(1);
    expect(posts[0].title).toBe("Hello World");
  });

  test("isConnected: check if user is connected to post", async () => {
    const connected = await db.users.isConnected({
      from: { name: "alice" },
      to: { id: "post-1" },
      relation: "posts",
    });

    expect(connected).toBe(true);
  });

  test("disconnect: remove post connection", async () => {
    const result = await db.users.disconnect({
      from: { name: "alice" },
      to: { id: "post-1" },
      relation: "posts",
    });

    expect(result).toBeDefined();
  });

  test("deleteOne: delete user", async () => {
    const result = await db.users.deleteOne({
      where: { name: "alice" },
    });

    expect(result).toBeDefined();
  });

  test("count: count users matching where", async () => {
    await db.users.createOne({
      data: {
        name: "bob",
        password: "123",
      },
    });

    const count = await db.users.count({
      password: {
        contains: "1",
      },
    });

    expect(typeof count).toBe("number");
    expect(count).toBeGreaterThan(0);
  });

  test("metadata: labels, types, props", async () => {
    const labels = await db.users.getNodeLabels();
    const relTypes = await db.users.getRelationshipTypes();
    const props = await db.users.getNodeProperties();
    const relProps = await db.users.getRelationshipProperties("HAS_POST");

    expect(Array.isArray(labels)).toBe(true);
    expect(Array.isArray(relTypes)).toBe(true);
    expect(Array.isArray(props)).toBe(true);
    expect(Array.isArray(relProps)).toBe(true);
  });

  test("createMany: batch create posts", async () => {
    const result = await db.posts.createMany({
      data: {
        id: "post-3",
        title: "Bulk Created Post",
      },
    });

    expect(result).toBeDefined();
  });

  test("deleteMany: remove all posts", async () => {
    const result = await db.posts.deleteMany();
    expect(result).toBeDefined();
  });
});
