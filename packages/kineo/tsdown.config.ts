import { defineConfig } from "tsdown";

export default defineConfig([
  {
    entry: [
      "src/index.ts",
      "src/ir.ts",
      "src/kit/index.ts",
      "src/adapters/neo4j.ts",
      "src/compilers/sql.ts",
      "src/compilers/cypher.ts",
    ],
    external: ["neo4j-driver"],
    sourcemap: true,
    minify: true,
  },
  {
    entry: ["src/kit/main.ts"],
    dts: false,
    minify: true,
  },
]);
