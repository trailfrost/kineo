import process from "node:process";
import { Neo4jAdapter } from "../src/adapters/neo4j";
import { defineSchema, field, relation, model } from "kineo/schema";
import Kineo from "kineo";

export const schema = defineSchema({
  users: model({
    name: field.string().id(),
    password: field.string().required(),
    posts: relation.to("Post").outgoing("posts").array(),
  }),

  posts: model({
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
