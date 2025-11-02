import { KineoKitErrorKind, KineoKitError } from "@/error";

import type { Kineo } from "@/client";
import { FieldDef, RelationDef, type Schema } from "@/schema";

import type { Jiti } from "jiti/lib/types";
import type { Adapter } from "@/adapter";

export interface FileExport {
  file: string;
  export: string;
}

export type ReferenceFn<T> = () => Promise<T> | T;
export type Reference<T> =
  | string
  | FileExport
  | T
  | Promise<T>
  | ReferenceFn<T>;

export interface KineoConfig {
  schema: Reference<Schema>;
  client: Reference<Kineo<any, any>>;
  migrations: string;
}

export function defineConfig(config: KineoConfig) {
  return config;
}

export interface ParsedConfig {
  schema: Schema;
  schemaMod?: FileExport;
  client: Kineo<any, any>;
  clientMod?: FileExport;
  migrations: string;
}

export async function parseConfig(
  jiti: Jiti,
  module: KineoConfig
): Promise<ParsedConfig | undefined> {
  const { exported: client, module: clientMod } = await extract(
    jiti,
    module.client
  );
  const { exported: schema, module: schemaMod } = await extract(
    jiti,
    module.schema
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

async function extract<T>(
  jiti: Jiti,
  ref: Reference<T>
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

export async function push(
  adapter: Adapter<any, any>,
  newSchema: Schema,
  force: boolean
) {
  if (!adapter.pull) throw new KineoKitError(KineoKitErrorKind.NoSupport);
  if (!adapter.push) throw new KineoKitError(KineoKitErrorKind.NoSupport);

  const prevSchema = await adapter.pull();
  const diff = getDiff(prevSchema, newSchema);

  if (diff.breaking.length > 0) {
    if (!force)
      throw new KineoKitError(KineoKitErrorKind.BreakingSchemaChange, diff);
  }

  await adapter.push(newSchema);
}

export interface SchemaDiff {
  breaking: string[];
  nonBreaking: string[];
}

export function getDiff(prev: Schema, cur: Schema): SchemaDiff {
  const breaking: string[] = [];
  const nonBreaking: string[] = [];

  const prevModels = Object.keys(prev);
  const curModels = Object.keys(cur);

  // Detect removed or new models
  for (const model of prevModels) {
    if (!cur[model]) {
      breaking.push(`Model "${model}" was removed`);
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
            `In model "${model}", field "${key}" changed kind from "${prevField.kind}" to "${curField.kind}"`
          );
        }

        if (prevField.isArray !== curField.isArray) {
          breaking.push(
            `In model "${model}", field "${key}" changed array flag (${prevField.isArray} → ${curField.isArray})`
          );
        }

        if (!prevField.isRequired && curField.isRequired) {
          breaking.push(`In model "${model}", field "${key}" became required`);
        } else if (prevField.isRequired && !curField.isRequired) {
          nonBreaking.push(
            `In model "${model}", field "${key}" became optional`
          );
        }
      } else if (bothRelations) {
        if (prevField.pointTo !== curField.pointTo) {
          breaking.push(
            `In model "${model}", relation "${key}" now points to "${curField.pointTo}" instead of "${prevField.pointTo}"`
          );
        }

        if (prevField.isArray !== curField.isArray) {
          breaking.push(
            `In model "${model}", relation "${key}" changed array flag (${prevField.isArray} → ${curField.isArray})`
          );
        }

        if (!prevField.isRequired && curField.isRequired) {
          breaking.push(
            `In model "${model}", relation "${key}" became required`
          );
        } else if (prevField.isRequired && !curField.isRequired) {
          nonBreaking.push(
            `In model "${model}", relation "${key}" became optional`
          );
        }

        if (prevField.relDirection !== curField.relDirection) {
          nonBreaking.push(
            `In model "${model}", relation "${key}" changed direction (${prevField.relDirection} → ${curField.relDirection})`
          );
        }
      } else if (prevField.constructor !== curField.constructor) {
        breaking.push(
          `In model "${model}", property "${key}" changed type (field ↔ relation)`
        );
      }
    }
  }

  return { breaking, nonBreaking };
}
