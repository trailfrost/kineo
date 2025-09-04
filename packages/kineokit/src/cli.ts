import path from "node:path";
import process from "node:process";
import crypto from "node:crypto";
import fs from "node:fs/promises";
import inquirer from "inquirer";
import { existsSync } from "node:fs";
import { Command } from "commander";
import { createJiti } from "jiti";
import type { Schema } from "kineo/schema";
import type { Adapter } from "kineo/adapter";
import type { KineoClient } from "kineo";
import * as kineokit from "./index.js";

const cwd = process.cwd();
const jiti = createJiti(cwd);

let config: kineokit.Config | undefined;

/**
 * Runs the CLI.
 */
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
          `[warning] It seems like the default export is \`undefined\` in ${file}. Ignoring.`,
        );
        continue;
      }
      if ("config" in module) config = module.config as kineokit.Config;
      else config = module as kineokit.Config;
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
        await fs.readFile(path.join(cwd, "package.json"), "utf-8"),
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
`,
      );

      console.log(`[info] Successfully created \`${file}\`!`);
    });

  program
    .command("push")
    .description(
      "Pushes schema to database, skipping migrations. Warns you for possible breaking changes.",
    )
    .action(async () => {
      if (!config) {
        console.error(
          "[fatal] This command requires a configuration file. Run `kineokit init` to create one.",
        );
        return;
      }

      const { client, schema } = await importFiles();
      await kineokit.push(client.adapter, schema, true);

      console.log("[info] Database schema pushed!");
    });

  program
    .command("migrate")
    .description("Generates migration files.")
    .action(async () => {
      if (!config) {
        console.error(
          "[fatal] This command requires a configuration file. Run `kineokit init` to create one.",
        );
        return;
      }

      const { client, schema } = await importFiles();

      const schemaDiff = await kineokit.diff(client.adapter, schema);
      const migrations = await kineokit.migrate(client.adapter, schemaDiff);

      for (const migration of migrations) {
        await fs.writeFile(`${Date.now()}.kn`, migration, "utf-8");
      }

      console.log("[info] Migrations generated!");
    });

  program
    .command("status")
    .description("Gets status for the migration files, if they exist.")
    .action(async () => {
      if (!config) {
        console.error(
          "[fatal] This command requires a configuration file. Run `kineokit init` to create one.",
        );
        return;
      }

      const files = await fs.readdir(config.migrationsDir, {
        withFileTypes: true,
      });
      const migrations = await Promise.all(
        files.map(
          async (migrations) =>
            await fs.readFile(
              path.join(migrations.parentPath, migrations.name),
              "utf-8",
            ),
        ),
      );
      const hashes = migrations.map((migration) =>
        crypto.hash("sha256", migration, "hex"),
      );

      const { client } = await importFiles();
      const statuses = await kineokit.status(
        client.adapter,
        migrations,
        hashes,
      );
      for (const index in statuses) {
        console.log(`${files[index]}: ${statuses[index]}`);
      }
    });

  program
    .command("deploy")
    .description("Sends migrations to database.")
    .action(async () => {
      if (!config) {
        console.error(
          "[fatal] This command requires a configuration file. Run `kineokit init` to create one.",
        );
        return;
      }

      const files = await fs.readdir(config.migrationsDir, {
        withFileTypes: true,
      });
      const migrations = await Promise.all(
        files.map(
          async (migrations) =>
            await fs.readFile(
              path.join(migrations.parentPath, migrations.name),
              "utf-8",
            ),
        ),
      );
      const hashes = migrations.map((migration) =>
        crypto.hash("sha256", migration, "hex"),
      );

      const { client } = await importFiles();
      const statuses = await kineokit.status(
        client.adapter,
        migrations,
        hashes,
      );
      for (const index in statuses) {
        if (statuses[index] !== "deployed") {
          await kineokit.deploy(
            client.adapter,
            migrations[index],
            hashes[index],
          );
          console.log(`[info] Deployed migration ${files[index]}!`);
        }
      }
    });

  await program.parseAsync();
}

/**
 * Imports the client and schema.
 * @returns The schema and client exports.
 */
async function importFiles() {
  if (config?.schemaFile === config?.clientFile) {
    const module = (await jiti.import(config!.clientFile)) as Record<
      string,
      unknown
    >;
    const schema = module[config!.schemaExport] as Schema;
    return {
      schema,
      client: module[config!.clientExport] as KineoClient<Schema, Adapter>,
    };
  } else {
    const schemaModule = (await jiti.import(config!.schemaFile)) as Record<
      string,
      Schema
    >;
    const clientModule = (await jiti.import(config!.clientFile)) as Record<
      string,
      KineoClient<Schema, Adapter>
    >;

    return {
      schema: schemaModule[config!.schemaExport],
      client: clientModule[config!.clientExport],
    };
  }
}

main();
