import { test } from "vitest";
import { Kineo, field, relation, node } from ".";

function exampleDb() {
  const User = node({
    name: field("STRING").id(),
    password: field("STRING").required(),
    posts: relation("Post").outgoing("HAS_POST"),
  });

  const Post = node({
    id: field("STRING").id(),
    title: field("STRING").required(),
    author: relation("User").incoming("HAS_POST"),
  });

  const database = Kineo({
    url: "bolt://localhost:7687",
    auth: {
      username: "neo4j",
      password: "password",
    },
    schema: { User, Post },
  });

  return database;
}

test("database connection", async () => {
  const db = exampleDb();
  await db.driver.getServerInfo();
});
