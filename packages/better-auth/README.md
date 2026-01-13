# Kineo + Better-Auth

A Better-Auth adapter and Kineo plugin for using Kineo with Better-Auth.

# Usage

**Install the adapter.** Run one of these commands.

```sh
npm install @kineojs/better-auth
yarn add @kineojs/better-auth
pnpm add @kineojs/better-auth
bun add @kineojs/better-auth
```

**Set it as your adapter.** Use Better-Auth's `adapter` property.

```ts
import { betterAuth } from "better-auth";
import { kineoAdapter } from "@kineojs/better-auth";
import { client } from "<your-database-client>";

export const auth = betterAuth({
  adapter: kineoAdapter(client),
  emailAndPassword: { enabled: true }, // example
});
```

**Add the Better-Auth schema.** Merge your current schema with the schema coming from the adapter.

```ts
import { defineSchema } from "kineo";
import { betterAuthSchema } from "@kineojs/better-auth";

export const schema = defineSchema({
  ...betterAuthSchema,
});
```

> [!WARNING]
> Currently, there is no way to use the Better-Auth CLI to generate the schema with the Kineo adapter. We're working on it!
