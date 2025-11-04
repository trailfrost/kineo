import { defineConfig } from "tsdown";
import pkg from "./package.json" with { type: "json" };

export default defineConfig({
  entry: Object.entries(pkg.exports).map(([, value]) =>
    value.import.replace("./dist", "./src").replace(".mjs", ".ts")
  ),
  tsconfig: "./tsconfig.app.json",
  format: ["esm"],
  dts: true,
});
