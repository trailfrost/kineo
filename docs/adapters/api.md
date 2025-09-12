---
sidebar_position: 1
---

# Adapter API

The adapter API is the way you create adapters for Kineo. An adapter is just a function that returns an object.

## Creating your own adapter

To create an adapter, create a function that returns the `Adapter` type from `kineo/adapter`.

```ts
import type { Adapter } from "kineo/adapter";

export default function MyAdapter(opts: unknown): Adapter {
  return {
    /* ... */
  };
}
```

Adapters must implement functions so both Kineo and KineoKit can interact with them. To implement them, return them in the adapter:

```ts
return {
  async getSchema() { /* ... */ } // Returns a `Schema` from `kineo/schema`
  async close() { /* ... */ } // Returns nothing
  async compile(ir) { /* ... */ } // Returns { command: string, params: Record<string, string> }
  async run(command, params) { /* ... */ } // Returns `QueryResult`
  async push(schema) { /* ... */ } // Returns nothing
  async migrate(diff) { /* ... */ } // Returns `string[]` of migrations
  async status(migrations, hashes) { /* ... */ } // Returns `Array<"deployed" | "pending">`
  async deploy(migration, hash) { /* ... */ } // Sends a migration to the database
};
```

### `getSchema`

Gets a schema from the database. This is used for diffing schemas and pulling the schema. This returns a `schema` from `kineo/schema`.

### `close`

Closes the database adapter. This might close transactions, drivers, sessions, etc.

### `compile`

Compiles an IR to your database's query language. Returns an object in a `{ command: string, params: Record<string, string> }` shape.

### `run`

Runs a command with the specified parameters in your database session. Returns an object in this shape:

```ts
Map<
  number | string,
  | null
  | boolean
  | number
  | bigint
  | string
  | string[]
  | number[]
  | boolean[]
  | Node
>;
```

Node is a class, imported from `kineo/adapter`, that has the following properties:

```ts
identity: number | bigint;
labels: string[];
properties: Record<string, unknown>;
elementId: string;
```

These are also the arguments to the constructor, in the same order.

### `push`

Pushes the current schema to the database. You do not need to warn the user — KineoKit does that for you.

### `migrate`

Returns migrations based on the diff between two schemas. Check [SchemaDiff API reference](/docs/api/schema#diff) for more details. Returns an array of `string`s.

### `status`

Gets migration statuses for each migration passed as a parameter, along with their SHA-256 hashes if necessary. Returns an array of strings that are either `deployed` or `pending`.

### `deploy`

Sends the migration to the database. Takes in the migration to send and its hash.
