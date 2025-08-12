# Kineo

Base package for Kineo. Contains API for adapters, model and schema creation and intermediate representation.

```ts
// This example uses the Neo4j adapter.
import Neo4jAdapter from "kineo-neo4j";
import Kineo from "kineo";
import {
  defineSchema,
  model,
  field,
  relation,
  type InferSchema,
} from "kineo/schema";

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

export type Schema = InferSchema<typeof schema>;

export const db = Kineo(
  Neo4jAdapter({
    url: "bolt://localhost:7687/",
    auth: {
      username: "neo4j",
      password: "password",
    },
  }),
  schema
);
```
