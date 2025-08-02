import "dotenv/config";
import type { KineoOGM } from "kineo";
import type { Schema } from "kineo/schema";
import { Command } from "commander";
import { createJiti } from "jiti";
import inquirer from "inquirer";
import fs from "node:fs/promises";

const jiti = createJiti(import.meta.url);

export type Config = {
  dbFile: string;
  dbExport: string;
  schemaFile: string;
  schemaExport: string;
};

async function tryImportConfig(): Promise<Config | null> {
  const possibleFiles = [
    "./kineo.config.ts",
    "./kineo.config.mts",
    "./kineo.config.cts",
    "./kineo.config.js",
    "./kineo.config.mjs",
    "./kineo.config.cjs",
  ];

  for (const possibleFile of possibleFiles) {
    try {
      return await jiti.import(possibleFile, { default: true });
    } catch {
      continue;
    }
  }

  throw new Error("Could not import config.");
}

async function setConfig(alwaysInquire: boolean): Promise<Config | null> {
  try {
    if (alwaysInquire) throw new Error("always inquire!");
    const result = await tryImportConfig();
    return result;
  } catch {
    if (!alwaysInquire) {
      const { canContinue } = await inquirer.prompt([
        {
          type: "confirm",
          name: "canContinue",
          message:
            "It seems like you don't have a configuration file. Would you like to set up one?",
        },
      ]);
      if (!canContinue) return null;
    }

    const { dbFile, dbExport, schemaFile, schemaExport } =
      await inquirer.prompt([
        {
          type: "input",
          name: "dbFile",
          message: "Where is your OGM located?",
          default: "./src/db/index.ts",
        },
        {
          type: "input",
          name: "dbExport",
          message: "What is the export name?",
          default: "default",
        },
        {
          type: "input",
          name: "schemaFile",
          message: "Where is your schema located?",
          default: "./src/db/schema.ts",
        },
        {
          type: "input",
          name: "schemaExport",
          message: "What is the export name?",
          default: "default",
        },
      ]);

    await fs.writeFile(
      "./kineo.config.ts",
      `import type { Config } from "kineokit";

// https://kineo.trailfrost.com/config
const config: Config = {
  dbFile: "${dbFile}",
  dbExport: "${dbExport}",
  schemaFile: "${schemaFile}",
  schemaExport: "${schemaExport}",
};

export default config;
`
    );

    return {
      dbFile: dbFile as string,
      dbExport: dbExport as string,
      schemaFile: schemaFile as string,
      schemaExport: schemaExport as string,
    };
  }
}

async function importFiles(config: Config) {
  let schema: Schema | null;
  try {
    const schemaImport = (await jiti.import(config.schemaFile)) as Record<
      string,
      Schema
    >;
    schema = schemaImport[config.schemaExport];
  } catch {
    schema = null;
  }

  let db: KineoOGM<Schema> | null;
  try {
    const dbImport = (await jiti.import(config.dbFile)) as Record<
      string,
      KineoOGM<Schema>
    >;
    db = dbImport[config.dbExport];
  } catch {
    db = null;
  }

  return { schema, db };
}

const program = new Command();

program.command("init").action(async () => {
  void (await setConfig(true));
});

program.command("push").action(async () => {
  const config = await setConfig(false);
  if (!config)
    return console.error("Program can't continue without configuration.");

  const { schema } = await importFiles(config);
  console.log("push schema", schema, "to db");
});

program.command("pull").action(async () => {
  const config = await setConfig(false);
  if (!config)
    return console.error("Program can't continue without configuration.");

  console.log("pull from db");
});

const migration = program.command("migration").action(async () => {
  const config = await setConfig(false);
  if (!config)
    return console.error("Program can't continue without configuration.");

  console.log("generate migrations");
});

migration.command("send").action(async () => {
  const config = await setConfig(false);
  if (!config)
    return console.error("Program can't continue without configuration.");

  console.log("send migrations");
});

migration.command("status").action(async () => {
  const config = await setConfig(false);
  if (!config)
    return console.error("Program can't continue without configuration.");

  console.log("get migration status");
});

program.parseAsync();
