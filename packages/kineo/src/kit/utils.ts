import crypto from "node:crypto";

import { KineoKitErrorKind, KineoKitError } from "@/error";
import type { Kineo } from "@/client";
import type { Adapter, MigrationEntry } from "@/adapter";
import { FieldDef, RelationDef, type Schema } from "@/schema";

import type { Jiti } from "jiti/lib/types";

/**
 * A file path based import in the configuration file.
 */
export interface FileExport {
  file: string;
  export: string;
}

/**
 * A reference function.
 */
export type ReferenceFn<T> = () => Promise<T> | T;
/**
 * A reference to a file.
 */
export type Reference<T> =
  | string
  | FileExport
  | T
  | Promise<T>
  | ReferenceFn<T>;

/**
 * High-level Kineo configuration.
 */
export interface KineoConfig {
  /**
   * The schema reference.
   */
  schema: Reference<Schema>;
  /**
   * The client reference.
   */
  client: Reference<Kineo<any, any>>;
  /**
   * The migrations directory.
   */
  migrations: string;
}

/**
 * Adds type definitions to an object.
 * @param config The configuration.
 * @returns The same configuration.
 */
export function defineConfig(config: KineoConfig) {
  return config;
}

/**
 * Low-level Kineo config.
 */
export interface ParsedConfig {
  schema: Schema;
  schemaMod?: FileExport;
  client: Kineo<any, any>;
  clientMod?: FileExport;
  migrations: string;
}

/**
 *
 * @param jiti The Jiti instance.
 * @param module The Kineo configuration.
 * @returns Parsed configuration, or undefined.
 */
export async function parseConfig(
  jiti: Jiti,
  module: KineoConfig,
): Promise<ParsedConfig> {
  const { exported: client, module: clientMod } = await extract(
    jiti,
    module.client,
  );
  const { exported: schema, module: schemaMod } = await extract(
    jiti,
    module.schema,
  );

  if (!client) {
    throw new KineoKitError(KineoKitErrorKind.MissingClient);
  }

  if (!schema) {
    throw new KineoKitError(KineoKitErrorKind.MissingSchema);
  }

  return {
    client,
    clientMod,
    schema,
    schemaMod,
    migrations: module.migrations,
  };
}

/**
 * Imports a module from a reference.
 * @param jiti The Jiti instance.
 * @param ref The reference to extract.
 * @returns The imported module.
 */
async function extract<T>(
  jiti: Jiti,
  ref: Reference<T>,
): Promise<{ module?: FileExport; exported?: T }> {
  if (isReferenceFn(ref)) {
    return { exported: await ref() };
  }

  if (ref instanceof Promise) {
    return { exported: await ref };
  }

  if (isFileExport(ref)) {
    const module = (await jiti.import(ref.file)) as { [ref.export]: T };
    const exported = module[ref.export];
    return {
      exported,
      module: {
        file: ref.file,
        export: ref.export,
      },
    };
  }

  return { exported: ref as T };
}

function isReferenceFn<T>(ref: Reference<T>): ref is ReferenceFn<T> {
  return typeof ref === "function";
}

function isFileExport(ref: Reference<any>): ref is FileExport {
  return typeof ref === "object" && "file" in ref && "export" in ref;
}

/**
 * Pushes a schema to the database.
 * @param adapter The adapter.
 * @param newSchema The new schema to push.
 * @param force To not throw an error on breaking changes.
 */
export async function push(
  adapter: Adapter<any, any>,
  newSchema: Schema,
  force?: boolean,
) {
  if (!adapter.pull) throw new KineoKitError(KineoKitErrorKind.NoSupport);
  if (!adapter.push) throw new KineoKitError(KineoKitErrorKind.NoSupport);

  if (!force) {
    const { schema: prevSchema } = await adapter.pull();
    const diff = getDiff(prevSchema, newSchema);

    if (diff.breaking.length > 0) {
      throw new KineoKitError(KineoKitErrorKind.BreakingSchemaChange, diff);
    }
  }

  await adapter.push(newSchema);
}

/**
 * Differences between two schemas.
 */
export interface SchemaDiff {
  /**
   * Descriptions of the breaking changes.
   */
  breaking: string[];
  /**
   * Descriptions of the non-breaking changes.
   */
  nonBreaking: string[];
}

/**
 * Calculates the difference between two schemas.
 * @param prev The previous schema.
 * @param cur The current schema.
 * @returns A diff between the two schemas.
 */
export function getDiff(prev: Schema, cur: Schema): SchemaDiff {
  const breaking: string[] = [];
  const nonBreaking: string[] = [];

  const prevModels = Object.keys(prev);
  const curModels = Object.keys(cur);

  // Detect removed or new models
  for (const model of prevModels) {
    if (!cur[model]) {
      breaking.push(`Model "${model}" was removed`);
    } else if (cur[model].$modelName !== prev[model].$modelName) {
      breaking.push(`Model "${model}" was renamed to ${cur[model].$modelName}`);
    }
  }

  for (const model of curModels) {
    if (!prev[model]) {
      nonBreaking.push(`Model "${model}" was added`);
    }
  }

  // Compare existing models
  for (const model of prevModels) {
    const prevDef = prev[model];
    const curDef = cur[model];
    if (!curDef) continue;

    const prevKeys = Object.keys(prevDef);
    const curKeys = Object.keys(curDef);

    // Detect removed or new fields/relations
    for (const key of prevKeys) {
      if (!curDef[key]) {
        breaking.push(`In model "${model}", property "${key}" was removed`);
      }
    }

    for (const key of curKeys) {
      if (!prevDef[key]) {
        nonBreaking.push(`In model "${model}", property "${key}" was added`);
      }
    }

    // Compare existing fields/relations
    for (const key of prevKeys) {
      const prevField = prevDef[key] as any;
      const curField = curDef[key] as any;
      if (!curField) continue;

      const bothFields =
        prevField instanceof FieldDef && curField instanceof FieldDef;
      const bothRelations =
        prevField instanceof RelationDef && curField instanceof RelationDef;

      if (bothFields) {
        if (prevField.kind !== curField.kind) {
          breaking.push(
            `In model "${model}", field "${key}" changed kind from "${prevField.kind}" to "${curField.kind}"`,
          );
        }

        if (prevField.isArray !== curField.isArray) {
          breaking.push(
            `In model "${model}", field "${key}" changed array flag (${prevField.isArray} -> ${curField.isArray})`,
          );
        }

        if (!prevField.isRequired && curField.isRequired) {
          breaking.push(`In model "${model}", field "${key}" became required`);
        } else if (prevField.isRequired && !curField.isRequired) {
          nonBreaking.push(
            `In model "${model}", field "${key}" became optional`,
          );
        }
      } else if (bothRelations) {
        if (prevField.pointTo !== curField.pointTo) {
          breaking.push(
            `In model "${model}", relation "${key}" now points to "${curField.pointTo}" instead of "${prevField.pointTo}"`,
          );
        }

        if (prevField.isArray !== curField.isArray) {
          breaking.push(
            `In model "${model}", relation "${key}" changed array flag (${prevField.isArray} -> ${curField.isArray})`,
          );
        }

        if (!prevField.isRequired && curField.isRequired) {
          breaking.push(
            `In model "${model}", relation "${key}" became required`,
          );
        } else if (prevField.isRequired && !curField.isRequired) {
          nonBreaking.push(
            `In model "${model}", relation "${key}" became optional`,
          );
        }

        if (prevField.relDirection !== curField.relDirection) {
          nonBreaking.push(
            `In model "${model}", relation "${key}" changed direction (${prevField.relDirection} -> ${curField.relDirection})`,
          );
        }
      } else if (prevField.constructor !== curField.constructor) {
        breaking.push(
          `In model "${model}", property "${key}" changed type (field ↔ relation)`,
        );
      }
    }
  }

  return { breaking, nonBreaking };
}

/**
 * Pulls a schema from the database.
 * @param adapter The adapter.
 * @returns The schema.
 */
export async function pull(adapter: Adapter<any, any>) {
  if (!adapter.pull) throw new KineoKitError(KineoKitErrorKind.NoSupport);
  const { schema, full } = await adapter.pull();
  if (!full) throw new KineoKitError(KineoKitErrorKind.NoSupport);
  return schema;
}

/**
 * Generates migrations based on two schemas.
 * @param adapter The adapter.
 * @param prevSchema The previous schema.
 * @param newSchema The current schema.
 * @returns The generated migrations.
 */
export async function generate(
  adapter: Adapter<any, any>,
  prevSchema: Schema,
  newSchema: Schema,
) {
  if (!adapter.generate) throw new KineoKitError(KineoKitErrorKind.NoSupport);
  return await adapter.generate(prevSchema, newSchema);
}

/**
 * Deploys a migration.
 * @param adapter The adapter.
 * @param migration The migration.
 */
export async function deploy(adapter: Adapter<any, any>, migration: string) {
  if (!adapter.deploy) throw new KineoKitError(KineoKitErrorKind.NoSupport);
  return await adapter.deploy(
    migration,
    crypto.hash("sha512", JSON.stringify(migration)),
  );
}

/**
 * Gets the status for a migration.
 * @param adapter The adapter.
 * @param migration The migration.
 * @returns If the migration has been deployed or not.
 */
export async function status(adapter: Adapter<any, any>, migration: string) {
  if (!adapter.status) throw new KineoKitError(KineoKitErrorKind.NoSupport);
  return await adapter.status(
    migration,
    crypto.hash("sha512", JSON.stringify(migration)),
  );
}

/**
 * A simple "up", "down" migration array.
 */
export type Migration = [string, string];

/**
 * Compiles migration entries to a simpler "up", "down" array.
 * @param entries The migration entries.
 * @returns The compiled object.
 */
export function compileEntries(entries: MigrationEntry[]): Migration {
  let up = "";
  let down = "";

  for (const entry of entries) {
    if (entry.type === "command") {
      up += `${entry.command}${entry.description ? ` -- ${entry.description}` : ""}\n\n`;
      if (entry.reverse) down += `${entry.reverse}\n\n`;
    } else if (entry.type === "note") {
      up += `${entry.description ? `-- ${entry.description}\n` : ""}-- ${entry.note}\n`;
      down += `-- Revert: ${entry.description ? `${entry.description}\n` : `-- ${entry.note}`}\n`;
    }
  }

  return [up, down];
}

/**
 * Decompiles a a simple "up", "down" array to migration entries.
 * @param array The compiled entries.
 * @returns The decompiled entries.
 */
export function decompileEntries([up, down]: Migration): MigrationEntry[] {
  const migrations: MigrationEntry[] = [];

  const upSplit = up.split("\n\n");
  const downSplit = down.split("\n\n");

  decompile(upSplit, migrations, "command");
  decompile(downSplit, migrations, "reverse");

  return migrations;
}

/**
 * Decompiles a string array into a migration entry array.
 * @param statements The list of commands.
 * @param migrations The migrations.
 * @param key The key to insert, either `command` or `reverse`.
 */
function decompile(
  statements: string[],
  migrations: MigrationEntry[],
  key: "command" | "reverse",
) {
  for (const stmt of statements) {
    const split = stmt.split("\n");
    for (let i = 0; i < split.length; i++) {
      const entry = split[i];
      // skip empty lines, to avoid creating blank commands
      if (!entry || entry.trim() === "") continue;

      if (entry.startsWith("--")) {
        if (key === "reverse") continue;

        let note: string;
        let description: string | undefined;
        if (i + 1 < split.length && split[i + 1].startsWith("--")) {
          description = entry;
          note = split[++i];
        } else {
          note = entry;
        }

        migrations.push({
          type: "note",
          description,
          note,
        });
      } else {
        const [command, description] = entry.split(" -- ");
        migrations.push({
          type: "command",
          description,
          [key]: command,
        } as any);
      }
    }
  }
}

export function filterEntries(
  entries: MigrationEntry[],
  key: "command" | "reverse",
) {
  return entries
    .filter((entry) => entry.type === "command")
    .map((entry) => entry[key])
    .join("\n");
}
