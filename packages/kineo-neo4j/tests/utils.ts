// @ts-expect-error `process` is a Node.js module
import process from "node:process";
import { defineSchema, field, relation, model } from "kineo/schema";
import Kineo from "kineo";
import Neo4jAdapter from "../src";

export const schema = defineSchema({
  User: model({
    name: field.string().id(),
    password: field.string().required(),
    posts: relation.to("Post").outgoing("posts").array(),
  }),

  Post: model({
    id: field.string().id(),
    title: field.string().required(),
    author: relation.to("User").incoming("posts"),
  }),
});

export const db = Kineo(
  Neo4jAdapter({
    url: "bolt://localhost:7687",
    auth: {
      username: "neo4j",
      password: "password",
    },
  }),
  schema,
);

await db.adapter.run(
  `
  MATCH (n)
  DETACH DELETE n
  `,
  {},
);

process.on("exit", db.close);
