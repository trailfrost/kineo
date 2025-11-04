import { defineConfig } from "tsdown";
import pkg from "./package.json" with { type: "json" };

export default defineConfig({
  entry: [
    ...Object.entries(pkg.bin).map(([, value]) => format(value)),
    ...Object.entries(pkg.exports).map(([, value]) => format(value.import)),
  ],
  tsconfig: "./tsconfig.app.json",
  format: ["esm"],
  dts: true,
  minify: true,
});

function format(str: string) {
  return str.replace("./dist", "./src").replace(".mjs", ".ts");
}
