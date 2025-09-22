import { describe, test, expect } from "vitest";
import { defineModel, defineSchema, field, relation } from "../src/schema";
import { Kineo, KineoClient, type InferClient } from "../src/client";
import { GraphModel, Model } from "../src/model";
import type { Adapter } from "../src/adapter";

// --- Setup test schema and adapter ---

const adapter: Adapter<Model> = {
  name: "example-adapter",
  Model: GraphModel,

  close() {},
  compile() {
    return { command: "", params: {} };
  },
  exec() {
    return new Map();
  },
};

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
});

describe("Kineo client", () => {
  const client = Kineo(adapter, schema);
  test("creates a client with models matching schema keys", () => {
    // should have the same keys as schema
    expect(Object.keys(client).sort()).toEqual(Object.keys(schema).sort());

    // every model key should be a Model instance
    expect(client.users).toBeInstanceOf(Model);
    expect(client.posts).toBeInstanceOf(Model);
    expect(client.schema).toEqual(schema);
  });

  test("schema property is preserved in client", () => {
    expect(client.schema).toBe(schema);
  });

  test("different models are independent instances", () => {
    expect(client.users).not.toBe(client.posts);
  });

  test("InferClient type inference works (compile-time)", () => {
    // purely type-level, but we can runtime-check shape loosely
    type ClientType = InferClient<KineoClient<typeof schema, typeof adapter>>;

    // runtime check: keys should exist
    const keys: (keyof ClientType)[] = ["users", "posts"];
    for (const k of keys) {
      expect(client).toHaveProperty(k);
    }
  });
});
