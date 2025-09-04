import { describe, expect, test } from "vitest";
import { defineSchema, field, model, relation } from "kineo/schema";
import { schema, adapter } from "./utils";
import * as kineokit from "../src";

describe("CLI commands test", () => {
  test("detects field and relationship differences between schemas", async () => {
    // Create a modified schema
    const modifiedSchema = defineSchema({
      User: model({
        // name changed type: string -> integer
        name: field.integer().id(),
        // bio removed
        // posts relationship changed: array -> single (removed .array())
        posts: relation.to("posts").outgoing("HAS_POSTS").default([]),
        // new field added
        age: field.integer().required(),
      }),
      // Post node removed completely
    });

    // Mock adapter so getSchema() returns the modified schema
    const mockAdapter = {
      ...adapter,
      getSchema: async () => modifiedSchema,
    };

    const result = await kineokit.diff(mockAdapter, schema);

    expect(result.kind).toBe("schema.diff");
    expect(result.severity).toBe("breaking");
    expect(result.nodeDiffs).toHaveLength(2);

    const userDiff = result.nodeDiffs.find(
      (d) =>
        d.kind === "node.changed" &&
        (d as { nodeName: string }).nodeName === "User",
    );
    expect(userDiff).toBeDefined();
    expect(userDiff).toMatchObject({
      kind: "node.changed",
      nodeName: "User",
      severity: "breaking",
    });

    expect(userDiff?.kind).toBe("node.changed");
    if (userDiff?.kind !== "node.changed") return;

    expect(userDiff?.fieldDiffs).toEqual(
      expect.arrayContaining([
        expect.objectContaining({ kind: "field.changedType" }),
        expect.objectContaining({ kind: "field.removed" }),
        expect.objectContaining({ kind: "field.added" }),
      ]),
    );

    expect(userDiff?.relationshipDiffs).toEqual(
      expect.arrayContaining([
        expect.objectContaining({ kind: "relationship.changedArray" }),
      ]),
    );

    const postDiff = result.nodeDiffs.find((d) => d.kind === "node.removed");
    expect(postDiff).toMatchObject({
      kind: "node.removed",
      severity: "breaking",
    });
  });

  test("pushing to database", async () => {
    await kineokit.push(adapter, schema);
  });

  test("deploying a migration", async () => {
    await kineokit.deploy(
      adapter,
      "CREATE TABLE users IF NOT EXISTS",
      "<pretend there's a hash here>",
    );
  });

  test("generating migrations", async () => {
    const migrations = await kineokit.migrate(adapter, {
      kind: "schema.diff",
      nodeDiffs: [],
      severity: "breaking",
    });

    expect(migrations).toEqual(["CREATE TABLE name IF NOT EXISTS"]);
  });

  test("getting migration statuses", async () => {
    const statuses = await kineokit.status(adapter, [], []);
    expect(statuses).toEqual(["deployed", "pending"]);
  });
});
