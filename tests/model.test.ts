import { describe, test, expect } from "vitest";
import { db } from "./utils";

describe("Model", () => {
  test("createOne: creates a user", async () => {
    const result = await db.User.createOne({
      data: {
        name: "alice",
        password: "secure123",
      },
    });

    expect(result).toEqual({
      name: "alice",
      password: "secure123",
    });
  });

  test("matchOne: match user by name with nested where", async () => {
    const user = await db.User.matchOne({
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
    const merged = await db.User.mergeOne({
      where: { name: "alice" },
      create: {
        name: "alice",
        password: "secure123",
      },
      update: {
        password: "updated!",
      },
    });

    expect(merged.name).toBe("alice");
    expect(merged.password).toBe("updated!");
  });

  test("connect: connect user to post", async () => {
    await db.Post.createOne({
      data: {
        id: "post-1",
        title: "Hello World",
      },
    });

    const result = await db.User.connect({
      from: { name: "alice" },
      to: { id: "post-1" },
      relation: "posts",
    });

    expect(result).toBeDefined();
  });

  test("getRelations: get posts for user", async () => {
    const posts = await db.User.getRelations({
      from: { name: "alice" },
      relation: "posts",
      where: {
        title: {
          contains: "Hello",
        },
      },
    });

    expect(posts.length).toBeGreaterThanOrEqual(1);
    expect(posts[0].title).toBe("Hello World");
  });

  test("upsertRelation: connect or create post", async () => {
    const result = await db.User.upsertRelation({
      from: { name: "alice" },
      to: { id: "post-2", title: "Another Post" },
      relation: "posts",
      create: true,
    });

    expect(result).toBeDefined();
  });

  test("isConnected: check if user is connected to post", async () => {
    const connected = await db.User.isConnected({
      from: { name: "alice" },
      to: { id: "post-1" },
      relation: "posts",
    });

    expect(connected).toBe(true);
  });

  test("deleteRelation: remove post connection", async () => {
    const result = await db.User.deleteRelation({
      from: { name: "alice" },
      to: { id: "post-1" },
      relation: "posts",
    });

    expect(result).toBeDefined();
  });

  test("deleteOne: delete user", async () => {
    const result = await db.User.deleteOne({
      where: { name: "alice" },
    });

    expect(result).toBeDefined();
  });

  test("count: count users matching where", async () => {
    await db.User.createOne({
      data: {
        name: "bob",
        password: "123",
      },
    });

    const count = await db.User.count({
      password: {
        contains: "1",
      },
    });

    expect(typeof count).toBe("number");
    expect(count).toBeGreaterThan(0);
  });

  test("metadata: labels, types, props", async () => {
    const labels = await db.User.getNodeLabels();
    const relTypes = await db.User.getRelationshipTypes();
    const props = await db.User.getNodeProperties("User");
    const relProps = await db.User.getRelationshipProperties("HAS_POST");

    expect(Array.isArray(labels)).toBe(true);
    expect(Array.isArray(relTypes)).toBe(true);
    expect(Array.isArray(props)).toBe(true);
    expect(Array.isArray(relProps)).toBe(true);
  });

  test("createMany: batch create posts", async () => {
    const result = await db.Post.createMany({
      data: {
        id: "post-3",
        title: "Bulk Created Post",
      },
    });

    expect(result).toBeDefined();
  });

  test("deleteMany: remove all posts", async () => {
    const result = await db.Post.deleteMany();
    expect(result).toBeDefined();
  });
});
