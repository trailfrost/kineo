# Kineo

Object-relation/graph mapper for TypeScript.

```ts
import { Kineo } from "kineo";
import { Neo4jAdapter } from "kineo/adapter/neo4j";
import {
  defineSchema,
  defineModel,
  field,
  relation,
  type InferSchema,
} from "kineo/schema";

export const schema = defineSchema({
  users: defineModel({
    name: field.string().id(),
    password: field.string().required(),
    posts: relation.to("Post").outgoing("HAS_POST").array(),
  }),

  posts: defineModel({
    id: field.string().id(),
    title: field.string().required(),
    author: relation.to("users").incoming("HAS_POST"),
  }),
});

export type Schema = InferSchema<typeof schema>;

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

const user = await db.users.findFirst({
  where: {
    name: {
      startsWith: "a",
      not: {
        endsWith: "z",
      },
    },
    AND: [
      {
        password: {
          contains: "secure",
        },
      },
    ],
  },
  include: {
    posts: true,
  },
});
```
