---
sidebar_position: 2
---

# Managing migrations

KineoKit is the migration manager for Kineo. It allows you to manage migrations of any adapter.

## Installation

To install KineoKit, you must have Node.js >= 12.17.1 installed.

Then, you can install KineoKit using a package manager:

```sh
npx kineokit init      # OR
pnpm dlx kineokit init # OR
yarn dlx kineokit init # OR
bunx kineokit init
```

## Setting Up KineoKit

Run the installation command above. It will ask you a few questions, install the necessary packages and generate a configuration file for you, that looks something like this:

```ts
import { defineConfig } from "kineokit";

// https://kineo.trailfrost.com/docs/ref/config
export default defineConfig({
  // ...
});
```

From there, you can add scripts to your `package.json` or just run `kineokit` directly:

```json
{
  "scripts": {
    "db": "kineokit",
    "db:push": "kineokit push",
    "db:pull": "kineokit pull"
    // ...
  }
}
```

That's it! You can now run some commands, KineoKit is set up.

## Commands

KineoKit has several commands.

- `kineokit init`: Creates configuration file and installs packages.
- `kineokit push`: Sends the current schema to the database.
- `kineokit pull`: Gets the current schema from the database. This overrides your current schema — use with caution.
- `kineokit migrate`: Generates migrations.
- `kineokit status`: Gets migration status for every migration file.
- `kineokit deploy`: Sends migrations to database.

It's important to note that not all adapters will support every single command. Check the adapter's documentation for which ones it supports.
