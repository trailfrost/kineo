import path from "node:path";
import process from "node:process";
import fs from "node:fs/promises";
import inquirer from "inquirer";
import { existsSync } from "node:fs";
import { Command } from "commander";
import { createJiti } from "jiti";
import type { Config } from "./index.js";

const cwd = process.cwd();
const jiti = createJiti(cwd);

let config: Config | undefined;

async function main() {
  const possibleFiles = [
    "kineo.config.ts",
    "kineo.config.js",
    "kineo.config.mts",
    "kineo.config.mjs",
    "kineo.config.cts",
    "kineo.config.cjs",
  ];
  for (const file of possibleFiles) {
    const fullPath = path.join(cwd, file);
    if (!existsSync(fullPath)) continue;

    try {
      const module = (await jiti.import(fullPath, { default: true })) as object;
      if (!module) {
        console.warn(
          `[warning] It seems like the default export is \`undefined\` in ${file}. Ignoring.`
        );
        continue;
      }
      if ("config" in module) config = module.config as Config;
      else config = module as Config;
    } catch (err) {
      console.warn(`[warning] Could not import ${file}: ${err}`);
    }
  }

  if (!config) {
    console.warn("[warning] We could not find a suitable configuration file.");
  }

  const program = new Command("kineokit").version("1.0.0");

  program
    .command("init")
    .description("Initializes by creating a configuration file.")
    .action(async () => {
      if (config) {
        console.log("[info] Kineo already initialized!");
        return;
      }

      const results = await inquirer.prompt([
        {
          type: "input",
          name: "migrationsDir",
          default: "./migrations",
          message: "Where will your migrations be located?",
        },
        {
          type: "input",
          name: "schemaFile",
          default: "./src/db/schema.ts",
          message: "Where is your schema located?",
        },
        {
          type: "input",
          name: "schemaExport",
          default: "default",
          message: "What is the name of the export for your schema?",
        },
        {
          type: "input",
          name: "clientFile",
          default: "./src/db/adapter.ts",
          message: "Where is your client located?",
        },
        {
          type: "input",
          name: "clientExport",
          default: "default",
          message: "What is the name of the export for your client?",
        },
        {
          type: "confirm",
          name: "isTypescript",
          default: existsSync(path.join(cwd, "tsconfig.json")),
          message: "Do you want your config file to be TypeScript?",
        },
      ]);

      const packageJson = JSON.parse(
        await fs.readFile(path.join(cwd, "package.json"), "utf-8")
      );
      const isModule = "type" in packageJson && packageJson.type === "module";

      const file = results.isTypescript
        ? isModule
          ? "kineo.config.ts"
          : "kineo.config.mts"
        : isModule
          ? "kineo.config.js"
          : "kineo.config.mjs";
      const fullPath = path.join(cwd, file);

      await fs.writeFile(
        fullPath,
        `import { defineConfig } from "kineokit";

// https://kineo.trailfrost.com/kit/config
export default defineConfig({
  schemaFile: ${JSON.stringify(results.schemaFile)},
  schemaExport: ${JSON.stringify(results.schemaExport)},
  clientFile: ${JSON.stringify(results.clientFile)},
  clientExport: ${JSON.stringify(results.clientExport)},
  migrationsDir: ${JSON.stringify(results.migrationsDir)},
});
`
      );

      console.log(`[info] Successfully created \`${file}\`!`);
    });

  program
    .command("push")
    .description(
      "Pushes schema to database, skipping migrations. Warns you for possible breaking changes."
    )
    .action(() => {
      if (!config) {
        console.error(
          "[fatal] This command requires a configuration file. Run `kineokit init` to create one."
        );
        return;
      }
      throw new Error("Not Implemented"); // TODO
    });

  program
    .command("pull")
    .description("Pulls schema from database.")
    .action(() => {
      if (!config) {
        console.error(
          "[fatal] This command requires a configuration file. Run `kineokit init` to create one."
        );
        return;
      }
      throw new Error("Not Implemented"); // TODO
    });

  program
    .command("migrate")
    .description("Generates migration files.")
    .action(() => {
      if (!config) {
        console.error(
          "[fatal] This command requires a configuration file. Run `kineokit init` to create one."
        );
        return;
      }
      throw new Error("Not Implemented"); // TODO
    });

  program
    .command("status")
    .description("Gets status for the migration files, if they exist.")
    .action(() => {
      if (!config) {
        console.error(
          "[fatal] This command requires a configuration file. Run `kineokit init` to create one."
        );
        return;
      }
      throw new Error("Not Implemented"); // TODO
    });

  program
    .command("deploy")
    .description("Sends migrations to database.")
    .action(() => {
      if (!config) {
        console.error(
          "[fatal] This command requires a configuration file. Run `kineokit init` to create one."
        );
        return;
      }
      throw new Error("Not Implemented"); // TODO
    });

  await program.parseAsync();
}

main();
