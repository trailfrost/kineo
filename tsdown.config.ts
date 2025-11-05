import { defineConfig } from "tsdown";
import pkg from "./package.json" with { type: "json" };

export default defineConfig({
  entry: [
    ...Object.values(pkg.bin).map(format),
    ...Object.values(pkg.exports).map((value) => format(value.import)),
  ],
  tsconfig: "./tsconfig.app.json",
  format: ["esm"],
  dts: true,
  minify: true,
});

function format(str: string) {
  return str.replace("./dist", "./src").replace(".mjs", ".ts");
}
