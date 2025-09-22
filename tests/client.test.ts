import { describe, test, expect } from "vitest";
import { Kineo, InferClient } from "../src/client";
import { defineModel, defineSchema, field, relation } from "../src/schema";
import { Model } from "../src/model";

// --- Setup test schema ---
const schema = defineSchema({
  users: defineModel({
    name: field.string().id(),
    bio: field.string(),
    posts: relation.to("posts").array().default([]),
  }),
  posts: defineModel({
    id: field.int().id(),
    created: field.datetime(),
    updated: field.timestamp(),
    author: relation.to("users").required(),
  }),
  schema: defineModel({}),
});

describe("Kineo client", () => {
  test("creates a client with models matching schema keys", () => {
    const client = Kineo(schema);

    // should have the same keys as schema
    expect(Object.keys(client).sort()).toEqual(Object.keys(schema).sort());

    // every model key should be a Model instance
    expect(client.users).toBeInstanceOf(Model);
    expect(client.posts).toBeInstanceOf(Model);
    expect(client.schema).toEqual(schema);
  });

  test("schema property is preserved in client", () => {
    const client = Kineo(schema);
    expect(client.schema).toBe(schema);
  });

  test("different models are independent instances", () => {
    const client = Kineo(schema);
    expect(client.users).not.toBe(client.posts);
  });

  test("InferClient type inference works (compile-time)", () => {
    // purely type-level, but we can runtime-check shape loosely
    type ClientType = InferClient<ReturnType<typeof Kineo<typeof schema>>>;
    const client = Kineo(schema);

    // runtime check: keys should exist
    const keys: (keyof ClientType)[] = ["users", "posts", "schema"];
    for (const k of keys) {
      expect(client).toHaveProperty(k);
    }
  });
});
