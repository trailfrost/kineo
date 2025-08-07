// @ts-expect-error `process` is a Node.js module
import process from "node:process";
import { defineSchema, field, relation, node } from "kineo/schema";
import Kineo from "kineo";

export const schema = defineSchema({
  User: node({
    name: field("STRING").id(),
    password: field("STRING").required(),
    posts: relation("Post").outgoing("posts").array(),
  }),

  Post: node({
    id: field("STRING").id(),
    title: field("STRING").required(),
    author: relation("User").incoming("posts"),
  }),
});

export const db = Kineo({
  url: "bolt://localhost:7687",
  auth: {
    username: "neo4j",
    password: "password",
  },
  schema,
});

await db.cypher(`
  MATCH (n)
  DETACH DELETE n
`);

process.on("exit", db.close);
