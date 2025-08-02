import type { Config } from "kineokit";

// https://kineo.trailfrost.com/config
const config: Config = {
  dbFile: "./src/db/index.ts",
  dbExport: "default",
  schemaFile: "./src/db/schema.ts",
  schemaExport: "default",
  migrationsDir: "./migrations",
};

export default config;
