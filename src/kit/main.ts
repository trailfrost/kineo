#!/usr/bin/env node
import { fileURLToPath } from "url";
import { dirname, resolve, extname } from "path";
import { createJiti } from "jiti";
import type { Command } from "convoker";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Only use Jiti in development (when running from .ts files)
const isTs = extname(__filename) === ".ts";

/**
 * The main function. Gets called when running KineoKit.
 */
async function main() {
  let program: Command;

  if (isTs) {
    // Run from source (dev)
    const jiti = createJiti(__dirname, {
      alias: { "@": resolve(__dirname, "../") },
    });
    ({ program } = (await jiti.import("./index.ts")) as Record<
      string,
      Command
    >);
  } else {
    // Run from compiled JS (prod)
    const mod = await import("./index.js");
    program = mod.program as any;
  }

  void program.run();
}

main();
