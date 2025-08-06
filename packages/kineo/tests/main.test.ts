import { describe, expect, test } from "vitest";
import { db } from "./utils";

describe("Connecting to the database", async () => {
  test("Running Cypher query", async () => {
    const { records } = await db.cypher(`
      MATCH (n)
      DETACH DELETE n
    `);

    expect(records.length).toBe(0);
  });
});
