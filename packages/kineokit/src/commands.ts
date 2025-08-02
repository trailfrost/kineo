import type { Schema } from "kineo/schema";

export function changes(now: Schema, prev: Schema) {
  console.log("comparing", now, "to", prev);
  // TODO
  return {
    diff: {},
    breaking: true,
  };
}

export async function push(schema: Schema) {
  console.log("pushing schema", schema);
  // TODO
}

export async function pull(schemaFile: string, schemaExport: string) {
  console.log("pulling schema to", schemaFile, "export", schemaExport);
  // TODO
}

export async function generateMigrations(
  outDir: string,
  now: Schema,
  prev: Schema
) {
  console.log(`generating migrations at ${outDir} (now ${now}, prev ${prev})`);
  // TODO
}

export async function sendMigrations(outDir: string) {
  console.log("sending migrations at", outDir);
  // TODO
}

export async function getMigrationStatus(
  outDir: string
): Promise<Record<string, boolean>> {
  console.log("getting migration status for", outDir);
  // TODO
  return {
    sent: true,
  };
}
