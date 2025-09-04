import {
  defineSchema,
  field,
  model,
  relation,
  type InferSchema,
} from "kineo/schema";
import Neo4jAdapter from "kineo/adapters/neo4j";
import Kineo from "kineo";
import config from "../kineo.config";

export const schema = defineSchema({
  users: model({
    name: field.string().id(),
    bio: field.string().required(),
    posts: relation.to("posts").outgoing("HAS_POSTS").array().default([]),
  }),

  posts: model({
    id: field.string().id(),
    title: field.string().required(),
    author: relation.to("users").incoming("HAS_POSTS").required(),
  }),
});

export type Schema = InferSchema<typeof schema>;

export const client = Kineo(
  Neo4jAdapter({
    url: "bolt://localhost:7687",
    auth: {
      username: "neo4j",
      password: "password",
    },
  }),
  schema,
);

export { config };
