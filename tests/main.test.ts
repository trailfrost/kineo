import { describe, expect, test } from "vitest";
import { db } from "./utils";

describe("Connecting to the database", async () => {
  test("Server information", async () => {
    const info = await db.driver.getServerInfo();
    expect(info).toMatchObject({
      address: "localhost:7687",
      agent: "Neo4j/2025.06.2",
      protocolVersion: 5.8,
    });
  });

  test("Running Cypher query", async () => {
    const { records } = await db.cypher(`
      MATCH (n)
      DETACH DELETE n
    `);

    expect(records.length).toBe(0);
  });
});
