# Kineo

Object-Graph Mapper for Neo4j in TypeScript.

```ts
import Kineo from "kineo";
import { defineSchema, node, field, relation, InferSchema } from "kineo/schema";

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

export type Schema = InferSchema<typeof schema>;

export const db = Kineo({
  url: "bolt://localhost:7687",
  auth: {
    username: "neo4j",
    password: "password",
  },
  schema,
});

const user = await db.User.matchOne({
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

## Contributing

Contributions are welcome! Just make sure to dicuss changes you want to make in [Discussions](https://github.com/trailfrost/kineo/discussions), and discuss bug reports in [Issues](https://github.com/trailfrost/kineo/issues).

## Roadmap

We plan on supporting other graph databases (like ArangoDB) in the future with adapters.

## License

This project uses the [MIT License](LICENSE).
