import type { KineoClient } from "kineo";
import type { Adapter, SchemaDiff } from "kineo/adapter";
import type { Schema } from "kineo/schema";

export interface Config {
  schemaFile: string;
  schemaExport: string;
  clientFile: string;
  clientExport: string;
  migrationsDir: string;
}

export function defineConfig(config: Config): Config {
  return config;
}

export async function push(
  client: KineoClient<Schema, Adapter>,
  schema: Schema
) {
  throw new Error("Not Implemented"); // TODO
}

export async function schemaDiff(
  client: KineoClient<Schema, Adapter>,
  schema: Schema
): Promise<SchemaDiff> {
  throw new Error("Not Implemented"); // TODO
}

export async function migrate(
  client: KineoClient<Schema, Adapter>,
  schema: SchemaDiff
): Promise<string[]> {
  throw new Error("Not Implemented"); // TODO
}

export async function status(
  migrations: string[],
  hashes: string[]
): Promise<Array<"pending" | "deployed">> {
  throw new Error("Not Implemented"); // TODO
}

export function deploy(migration: string, hash: string) {
  throw new Error("Not Implemented"); // TODO
}
