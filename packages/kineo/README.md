# Kineo

Object-Relation/Graph Mapper (ORM/OGM) for TypeScript.

# Usage

**Install Kineo.** Run one of these commands to install Kineo as a dependency:

```sh
npm install kineo
yarn add kineo
pnpm add kineo
bun add kineo
```

**Define a schema.** This schema can be anywhere in your codebase, as long as you can reference it.

```ts
import { defineSchema, model, field, relation, type InferSchema } from "kineo";

export const schema = defineSchema({
  users: model("User", {
    name: field.string().id(),
    password: field.string().required(),
    posts: relation.to("Post").outgoing("HAS_POST").array(),
  }),

  posts: model("Post", {
    id: field.string().id(),
    title: field.string().required(),
    author: relation.to("User").incoming("HAS_POST"),
  }),
});

export type Schema = InferSchema<typeof schema>;
```

**Create a client.** Use an adapter, which is the set of functions that converts Kineo's representation to your database's query language. Here, we use the Neo4j adapter, which requires you to install `neo4j-driver` as a dependency.

```ts
import { Kineo } from "kineo";
import { Neo4jAdapter } from "kineo/neo4j";

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
```

Different adapters will have different parameters.

**Query your database.** Kineo uses objects for querying.

```ts
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

## Migrations

Kineo also includes a migration manager by default. You can initialize it by running one of these commands after installation:

```sh
npx kineo init
yarn kineo init
pnpm kineo init
bunx kineo init
```

### Manual setup

Create a `kineo.config.ts` at the root of your project, and paste these contents:

```ts
import { defineConfig } from "kineo/kit";

export default defineConfig({
  schema: import("<your schema path>").then((mod) => mod["default"]),
  client: import("<your client path>").then((mod) => mod["default"]),
  migrations: "./migrations",
});
```

Replace `<your schema path>` and `default` with your schema file path and export name, and do the same for the client.

# License information

Kineo uses the MIT License. See [LICENSE](LICENSE) for more details.
