// @ts-expect-error `process` is a Node.js module
import process from "node:process";
import { defineSchema, field, relation, node } from "../src/schema";
import Kineo from "../src";

export const schema = defineSchema({
  User: node({
    name: field("STRING").id(),
    password: field("STRING").required(),
    posts: relation("Post").outgoing("HAS_POST").array(),
  }),

  Post: node({
    id: field("STRING").id(),
    title: field("STRING").required(),
    author: relation("User").incoming("HAS_POST"),
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
