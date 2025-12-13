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
    tsconfig: "tsconfig.app.json",
    format: ["esm"],
    external: ["neo4j-driver"],
    dts: true,
  },
  {
    entry: ["src/kit/main.ts"],
    tsconfig: "tsconfig.app.json",
    dts: false,
    minify: true,
  },
]);
