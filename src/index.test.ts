import { test } from "vitest";
import { Kineo } from ".";
import { defineSchema, field, relation, node } from "./schema";

// ! WARNING: to run these tests, you must have a Neo4j instance on localhost:7687. I recommend using Docker.

function exampleDb() {
  const schema = defineSchema({
    User: node({
      name: field("STRING").id(),
      password: field("STRING").required(),
      posts: relation("Post").outgoing("HAS_POST"),
    }),

    Post: node({
      id: field("STRING").id(),
      title: field("STRING").required(),
      author: relation("User").incoming("HAS_POST"),
    }),
  });

  const db = Kineo({
    url: "bolt://localhost:7687",
    auth: {
      username: "neo4j",
      password: "password",
    },
    schema,
  });

  return db;
}

test("database connection", async () => {
  const db = exampleDb();
  await db.driver.getServerInfo();

  await db.driver.close();
});
