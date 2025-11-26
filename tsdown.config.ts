import { defineConfig } from "tsdown";

export default defineConfig([
  {
    entry: [
      "src/index.ts",
      "src/kit/index.ts",
      "src/adapter/neo4j.ts",
      "src/compiler/sql.ts",
      "src/compiler/cypher.ts",
    ],
    tsconfig: "./tsconfig.app.json",
    format: ["esm"],
    dts: true,
  },
  {
    entry: ["src/kit/main.ts"],
    dts: false,
    minify: true,
  },
]);
