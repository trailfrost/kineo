import { defineConfig } from "./src";

// https://kineo.trailfrost.com/kit/config
export default defineConfig({
  schemaFile: "./tests/utils.ts",
  schemaExport: "schema",
  clientFile: "./tests/utils.ts",
  clientExport: "client",
  migrationsDir: "./migrations",
});
