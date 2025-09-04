# Kineo

Object-Relation Mapper and Object-Graph Mapper in TypeScript.

```ts
import Kineo from "kineo";
import Neo4jAdapter from "kineo/adapters/neo4j";
import {
  defineSchema,
  model,
  field,
  relation,
  InferSchema,
} from "kineo/schema";

export const schema = defineSchema({
  User: model({
    name: field.string().id(),
    password: field.string().required(),
    posts: relation.to("Post").outgoing("HAS_POST").array(),
  }),

  Post: model({
    id: field.string().id(),
    title: field.string().required(),
    author: relation.to("User").incoming("HAS_POST"),
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

const user = await db.User.findOne({
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

## License

This project uses the [MIT License](LICENSE).
