import { describe, test, expect } from "vitest";
import {
  field,
  relation,
  model,
  defineSchema,
  type InferSchema,
} from "../src/schema";

describe("FieldDef", () => {
  test("creates a simple field", () => {
    const f = field.string();
    expect(f.fieldType).toBe("STRING");
    expect(f.isRequired).toBe(false);
    expect(f.isArray).toBe(false);
    expect(f.isPrimaryKey).toBe(false);
  });

  test("applies id()", () => {
    const f = field.integer().id();
    expect(f.isPrimaryKey).toBe(true);
    expect(f.isRequired).toBe(true);
  });

  test("applies required()", () => {
    const f = field.bool().required();
    expect(f.isRequired).toBe(true);
  });

  test("applies optional()", () => {
    const f = field.bool().required().optional();
    expect(f.isRequired).toBe(false);
  });

  test("applies array()", () => {
    const f = field.integer().array();
    expect(f.isArray).toBe(true);
  });

  test("applies default()", () => {
    const f = field.integer().default(42);
    expect(f.defaultValue).toBe(42);
    expect(f.isRequired).toBe(true);
  });
});

describe("RelationshipDef", () => {
  test("creates a basic relationship", () => {
    const r = relation.to("User");
    expect(r.refTo).toBe("User");
    expect(r.refDirection).toBe("OUT");
    expect(r.refLabel).toBe("UNNAMED");
  });

  test("sets label", () => {
    const r = relation.to("User").label("KNOWS");
    expect(r.refLabel).toBe("KNOWS");
  });

  test("sets directions", () => {
    expect(relation.to("A").incoming("X").refDirection).toBe("IN");
    expect(relation.to("A").outgoing("Y").refDirection).toBe("OUT");
    expect(relation.to("A").both("Z").refDirection).toBe("BOTH");
  });

  test("applies required()", () => {
    const r = relation.to("User").required();
    expect(r.isRequired).toBe(true);
  });

  test("applies array()", () => {
    const r = relation.to("User").array();
    expect(r.isArray).toBe(true);
  });

  test("applies default()", () => {
    const r = relation.to("User").default("defaultVal");
    expect(r.defaultValue).toBe("defaultVal");
  });

  test("attaches metadata with meta()", () => {
    const r = relation.to("User").meta({ weight: 5 });
    expect(r.metadata).toEqual({ weight: 5 });
  });
});

describe("Utility functions", () => {
  test("node() returns input as-is", () => {
    const n = model({
      name: field.string(),
    });
    expect(n.name.fieldType).toBe("STRING");
  });

  test("defineSchema() returns schema unchanged", () => {
    const s = defineSchema({
      User: {
        id: field.integer().id(),
      },
    });
    expect(s.User.id.isPrimaryKey).toBe(true);
  });
});

describe("Type inference (InferSchema)", () => {
  // eslint-disable-next-line
  const schema = defineSchema({
    User: {
      id: field.integer().id(),
      name: field.string().required(),
      friends: relation.to("User").array().label("FRIENDS_WITH"),
    },
  });

  type Inferred = InferSchema<typeof schema>;

  test("infers types correctly", () => {
    const user: Inferred["User"] = {
      id: 1,
      name: "Alice",
      friends: [],
    };

    expect(user.id).toBeTypeOf("number");
    expect(user.name).toBeTypeOf("string");
    expect(Array.isArray(user.friends)).toBe(true);
  });

  test("allows optional fields", () => {
    // eslint-disable-next-line
    const schema = defineSchema({
      Post: {
        title: field.string().optional(),
      },
    });

    type Post = InferSchema<typeof schema>["Post"];

    const valid: Post = {};
    expect(valid).toBeDefined();
  });
});
