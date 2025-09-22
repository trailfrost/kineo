import { describe, test, expect } from "vitest";
import {
  field,
  relation,
  FieldDef,
  RelationDef,
  defineSchema,
  defineModel,
} from "../src/schema";

describe("FieldDef", () => {
  test("initialize with correct kind and optional name", () => {
    const f = new FieldDef("string", "username");
    expect(f.kind).toBe("string");
    expect(f.rowName).toBe("username");
    expect(f.isRequired).toBe(false);
    expect(f.isArray).toBe(false);
    expect(f.defaultValue).toBeUndefined();
  });

  test("chain methods and update properties", () => {
    const f = field.int("age").required().array().default(0);
    expect(f.kind).toBe("int");
    expect(f.rowName).toBe("age");
    expect(f.isRequired).toBe(true);
    expect(f.isArray).toBe(true);
    expect(f.defaultValue).toBe(0);
  });

  test("allow switching between required/optional and array/single", () => {
    const f = field.string("email").required().optional().single();
    expect(f.isRequired).toBe(false);
    expect(f.isArray).toBe(false);
  });
});

describe("RelationDef", () => {
  test("initialize with target and optional name", () => {
    const r = new RelationDef("User", "follows");
    expect(r.pointTo).toBe("User");
    expect(r.relName).toBe("follows");
    expect(r.relDirection).toBe("both");
  });

  test("chain methods and update properties", () => {
    const r = relation
      .to("Post", "likes")
      .outgoing("LIKES")
      .required()
      .array()
      .default([]);
    expect(r.pointTo).toBe("Post");
    expect(r.relName).toBe("likes");
    expect(r.relLabel).toBe("LIKES");
    expect(r.relDirection).toBe("outgoing");
    expect(r.isRequired).toBe(true);
    expect(r.isArray).toBe(true);
    expect(r.defaultValue).toEqual([]);
  });

  test("allow changing direction with labels", () => {
    const r = relation.to("Comment").incoming("HAS_COMMENT");
    expect(r.relDirection).toBe("incoming");
    expect(r.relLabel).toBe("HAS_COMMENT");
  });
});

describe("Schema utilities", () => {
  test("defineSchema return the same schema object", () => {
    const schema = defineSchema({
      User: defineModel({
        id: field.int("id").required(),
        name: field.string("name"),
      }),
    });
    expect(schema.User.id.kind).toBe("int");
    expect(schema.User.name.kind).toBe("string");
  });
});
