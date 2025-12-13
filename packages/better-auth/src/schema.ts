import fs from "fs/promises";
import type { BetterAuthDBSchema } from "better-auth/db";

export async function createSchema({
  tables,
  file,
}: {
  tables: BetterAuthDBSchema;
  file?: string;
}) {
  const { client, schema } = await deleteClientAndSchema(file);

  const tableBuilder: string[] = [];
  const tableIndent = " ".repeat(2);
  for (const key in tables) {
    const table = tables[key];
    const fields: string[] = [];
    const fieldIndent = " ".repeat(4);
    for (const key in table.fields) {
      const field = table.fields[key];

      const fieldBuilder: string[] = [];
      if (field.references)
        fieldBuilder.push(`relation.to(${field.references.model})`);
      else
        fieldBuilder.push(
          `field.${typeof field.type === "string" ? (field.type.endsWith("[]") ? `${field.type}().array()` : `${field.type}()`) : "string().array()"}`
        );

      if (field.fieldName) fieldBuilder.push(`name(${field.fieldName})`);
      if (field.defaultValue)
        fieldBuilder.push(
          `default(${typeof field.defaultValue === "function" ? field.defaultValue() : field.defaultValue})`
        );
      if (field.index) fieldBuilder.push(`index()`);
      if (field.required) fieldBuilder.push("required()");
      fields.push(`${fieldIndent}${key}: ${fieldBuilder.join(".")},`);
    }

    tableBuilder.push(`${tableIndent}${key}: model("${table.modelName}", {
${fields.join("\n")}
${fieldIndent}})`);
  }

  return {
    code: schema?.default
      ? `export default defineSchema({
${schema?.contents ? schema.contents : ""}
${tableBuilder.join("\n\n")}
});
`
      : `${client ? `${client}\n\n` : ""}export const ${schema?.exportName ?? "schema"} = defineSchema({
${schema?.contents ? schema.contents : ""}
${tableBuilder.join("\n\n")}
});
`,
    path: file ?? "./src/db.ts",
    append: true,
  };
}

export interface ClientAndSchema {
  schema?: {
    default: boolean;
    contents: string;
    exportName: string;
  };
  client?: string;
}

export async function deleteClientAndSchema(
  file?: string
): Promise<ClientAndSchema> {
  if (!file) return {};

  // TODO
  return {};
}
