import path from "node:path";
import process from "node:process";

import inquirer from "inquirer";
import { Command } from "commander";
import { createJiti } from "jiti";

import type { FileExport, KineoConfig } from "./index.js";
import type { KineoClient } from "@/client.js";
import type { Schema } from "@/schema.js";

const CWD = process.cwd();
const jiti = createJiti(CWD);

interface Config {
  schema: Schema;
  schemaFile?: FileExport;
  client: KineoClient<any, any>;
  clientFile?: FileExport;
  migrations: string;
}

let config: Config | undefined;

const program = new Command();

program.action(async () => {
  console.log(path.resolve("."));
  await inquirer.prompt([]);
  console.log(config);
});

async function main() {
  const files = [
    "kineo.config.ts",
    "kineo.config.js",
    "kineo.config.mts",
    "kineo.config.mjs",
    "kineo.config.cts",
    "kineo.config.cjs",
  ];
  for (const file of files) {
    try {
      const module = (await jiti.import(file, {
        default: true,
      })) as KineoConfig;
      config = await extractConfigFromMod(module);
      break;
    } catch {
      continue;
    }
  }

  await program.parseAsync();
}

async function extractConfigFromMod(module: KineoConfig): Promise<Config> {
  let client: KineoClient<any, any>;
  let clientFile: FileExport | undefined;
  if (typeof module.client === "function") {
    client = await module.client();
  } else if (typeof module.client === "object") {
    const mod = (await jiti.import(module.client.file)) as Record<
      string,
      KineoClient<any, any>
    >;
    if (!(module.client.export in module)) {
      throw new Error(
        `Expected Kineo client file '${module.client.file}' to have export '${module.client.export}'`
      );
    }
    client = mod[module.client.export];
    clientFile = module.client;
  } else {
    client = (await jiti.import(module.client, {
      default: true,
    })) as KineoClient<any, any>;
    clientFile = { file: module.client, export: "default" };
  }

  let schema: Schema;
  let schemaFile: FileExport | undefined;

  if (typeof module.schema === "function") {
    schema = await module.schema();
  } else if (typeof module.schema === "object") {
    const mod = (await jiti.import(module.schema.file)) as Record<
      string,
      Schema
    >;
    if (!(module.schema.export in module)) {
      throw new Error(
        `Expected Kineo schema file '${module.schema.file}' to have export '${module.schema.export}'`
      );
    }

    schema = mod[module.schema.export];
    schemaFile = module.schema;
  } else {
    schema = (await jiti.import(module.schema, { default: true })) as Schema;
    schemaFile = { file: module.schema, export: "default" };
  }

  return {
    client,
    clientFile,
    schema,
    schemaFile,
    migrations: module.migrations,
  };
}

void main();
