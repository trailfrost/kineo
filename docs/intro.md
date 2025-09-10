---
sidebar_position: 1
---

# Getting started

Kineo is a pluggable library for TypeScript that allows you to talk to SQL and NoSQL relational databases. This page will walk you through setting it up in your project.

## Installation

You must have Node.js >= 12.17.1 installed.

Then, you can install Kineo through a package manager:

```sh
npm install kineo        # OR
pnpm add kineo           # OR
yarn add kineo           # OR
bun add kineo
```

If you also want to install KineoKit (the migration manager), you can follow [this guide](/docs/kit) to get started.

## Usage

Create a schema, anywhere on your project. We will use `src/db.ts` here. You can use the `defineSchema`, `model`, `field` and `relation` helpers from `kineo/schema` to define a schema, and the `InferSchema` helper to get types out of your schema.

```ts
import {
  defineSchema,
  model,
  field,
  relation,
  type InferSchema,
} from "kineo/schema";

export const schema = defineSchema({
  // ...
});

export type Schema = InferSchema<typeof schema>;
```

Then, create models inside of your schema, using the `model`, `field` and `relation` helpers:

```ts
defineSchema({
  users: model({
    name: field.string().id(),
    password: field.string(),
    posts: relation.to("posts").array().outgoing("HAS_POSTS").default([]),
  }),

  posts: model({
    id: field.string().id(),
    title: field.string(),
    content: field.string(),
    author: relation.to("users").incoming("HAS_POSTS").required(),
  }),
});
```

Then, create your database client. You need an adapter for this. Kineo is an early-stage library, and only has an adapter and compiler for Neo4j at the moment. We're working on adapters for popular SQL and graph databases.

> Here are the states of the adapters we plan on implementing:
>
> - [x] Neo4j: Completed
> - [ ] PostgreSQL: Not completed
> - [ ] MySQL: Not completed
> - [ ] SQL Server: Not completed
> - [ ] SQLite: Not completed

To create a client, you can use the `Kineo` factory function from `kineo`.

```ts
import Kineo from "kineo";

// ...

export const client = Kineo(adapter, schema);
```

The adapter can come from Kineo itself (`kineo/adapters/*`) or from another npm package. Adapters are just functions, and each adapter takes different parameters.

Then, you can start using it. You pass query options to your models, like this:

```ts
import { client } from "@/db";

const user = await client.users.findOne({
  where: {
    name: {
      startsWith: "alice",
    },
  },
  select: {
    name: true,
    password: true,
    posts: {
      id: true,
      title: true,
      description: true,
    },
  },
});

console.log(user); // -> { name: "alice44", password: "correct horse battery staple", posts: [] }
```

And that's it! Kineo is now set up and ready to go, in your code.
