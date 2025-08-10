import js from "@eslint/js";
import globals from "globals";
import tseslint from "typescript-eslint";
import { defineConfig, globalIgnores } from "eslint/config";

export default defineConfig([
  {
    files: ["**/*.{js,mjs,cjs,ts,mts,cts}"],
    plugins: { js },
    extends: ["js/recommended"],
    languageOptions: { globals: globals.browser },
  },
  // @ts-expect-error tseslint doesn't support eslint 9.32 yet, and this works fine
  tseslint.configs.recommended,
  globalIgnores(["**/dist/**/*"]),
]);
