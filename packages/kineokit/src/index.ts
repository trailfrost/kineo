import "dotenv/config";
import type { KineoOGM } from "kineo";
import type { Schema } from "kineo/schema";
import { Command } from "commander";
import { createJiti } from "jiti";
import inquirer from "inquirer";
import fs from "node:fs/promises";
import process from "node:process";
import { existsSync } from "node:fs";
import {
  changes,
  push,
  pull,
  generateMigrations,
  sendMigrations,
  getMigrationStatus,
} from "./commands.js";

const jiti = createJiti(process.cwd());

export type Config = {
  dbFile: string;
  dbExport: string;
  schemaFile: string;
  schemaExport: string;
  migrationsDir: string;
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
      return (await jiti.import(possibleFile, { default: true })) || null;
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

    const { dbFile, dbExport, schemaFile, schemaExport, migrationsDir } =
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
        {
          type: "input",
          name: "migrationsDir",
          message: "Where do you want migrations to be stored?",
          default: "./migrations",
        },
      ]);

    await fs.mkdir("./.kineo", { recursive: true });

    await Promise.all([
      fs.writeFile(
        "./kineo.config.ts",
        `import type { Config } from "kineokit";

// https://kineo.trailfrost.com/config
const config: Config = {
  dbFile: "${dbFile}",
  dbExport: "${dbExport}",
  schemaFile: "${schemaFile}",
  schemaExport: "${schemaExport}",
  migrationsDir: "${migrationsDir}",
};

export default config;
`
      ),
      fs.writeFile("./.kineo/prevSchema.json", "{}"),
      // Append to ignore files
      existsSync("./.gitignore") &&
        fs.appendFile("./.gitignore", "\n\n# Kineo state\n.kineo/\n"),
      existsSync("./.hgignore") &&
        fs.appendFile("./.hgignore", "\n\n# Kineo state\n.kineo/\n"),
      existsSync("./.bzrignore") &&
        fs.appendFile("./.bzrignore", "\n\n# Kineo state\n.kineo/\n"),
      existsSync("./.p4ignore") &&
        fs.appendFile("./.p4ignore", "\n\n# Kineo state\n.kineo/\n"),
      existsSync("./cvsignore") &&
        fs.appendFile("./.cvsignore", "\n\n# Kineo state\n.kineo/\n"),
      existsSync("./.fossil-settings/.ignore") &&
        fs.appendFile(
          "./.fossil-settings/.ignore",
          "\n\n# Kineo state\n.kineo/\n"
        ),
      existsSync(".tfignore") &&
        fs.appendFile(".tfignore", "\n\n# Kineo state\n.kineo/\n"),
      existsSync(".rcsignroe") &&
        fs.appendFile(".rcsignroe", "\n\n# Kineo state\n.kineo/\n"),
    ]);

    return {
      dbFile: dbFile as string,
      dbExport: dbExport as string,
      schemaFile: schemaFile as string,
      schemaExport: schemaExport as string,
      migrationsDir: migrationsDir as string,
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

  const prevSchema = JSON.parse(
    await fs.readFile("./.kineo/prevSchema.json", "utf-8")
  );
  const { schema } = await importFiles(config);

  if (!schema) {
    console.warn(
      `It seems like you don't have a schema. Make it's located at "${config.schemaFile}" and exported as ${config.schemaExport}.`
    );
    console.log("Push cancelled.");
    return;
  }

  const { breaking } = changes(schema, prevSchema);
  if (breaking) {
    console.warn(
      "WARNING: Breaking changes detected. THIS MIGHT CAUSE DATA LOSS."
    );
    console.warn(
      "WARNING: A breaking change is making an optional field required, or removing a node, for example."
    );
    const { canContinue } = await inquirer.prompt([
      {
        type: "confirm",
        name: "canContinue",
        message: "Are you sure you want to continue?",
      },
    ]);

    if (!canContinue) {
      console.log("Push cancelled.");
      return;
    }
  }

  await push(schema);
  console.log("Schema successfully pushed.");
});

program.command("pull").action(async () => {
  const config = await setConfig(false);
  if (!config)
    return console.error("Program can't continue without configuration.");

  console.warn("WARNING: This will replace your current schema.");
  const { canContinue } = await inquirer.prompt([
    {
      type: "confirm",
      name: "canContinue",
      message: "Are you sure you want to continue?",
    },
  ]);
  if (!canContinue) return;

  await pull(config.schemaFile, config.schemaExport);
  console.log("Schema successfully pulled.");
});

const migrate = program.command("migrate").action(async () => {
  const config = await setConfig(false);
  if (!config)
    return console.error("Program can't continue without configuration.");

  const { schema } = await importFiles(config);
  const prevSchema = JSON.parse(
    await fs.readFile("./.kineo/prevSchema.json", "utf-8")
  );
  if (!schema) {
    console.warn(
      `It seems like you don't have a schema. Make it's located at "${config.schemaFile}" and exported as ${config.schemaExport}.`
    );
    console.log("Migration generation cancelled.");
    return;
  }

  await generateMigrations(config.migrationsDir, schema, prevSchema);
  console.log(
    "Migrations successfully generated. Use `kineokit migrate send` to push them to the database."
  );
});

migrate.command("send").action(async () => {
  const config = await setConfig(false);
  if (!config)
    return console.error("Program can't continue without configuration.");

  await sendMigrations(config.migrationsDir);
  console.log("Migrations successfully sent.");
});

migrate.command("status").action(async () => {
  const config = await setConfig(false);
  if (!config)
    return console.error("Program can't continue without configuration.");

  const statuses = await getMigrationStatus(config.migrationsDir);
  for (const key in statuses) {
    console.log(`${key}: ${statuses[key] ? "Already sent." : "Not sent yet."}`);
  }
});

program.parseAsync();
