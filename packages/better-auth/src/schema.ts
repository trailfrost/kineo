import fs from "fs/promises";
import type { BetterAuthDBSchema } from "better-auth/db";

// TODO make this safer by returning no code and just replacing the schema definition in place

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
          `field.${typeof field.type === "string" ? (field.type.endsWith("[]") ? `${field.type}().array()` : `${field.type}()`) : "string().array()"}`,
        );

      if (field.fieldName) fieldBuilder.push(`name(${field.fieldName})`);
      if (field.defaultValue)
        fieldBuilder.push(
          `default(${typeof field.defaultValue === "function" ? field.defaultValue() : field.defaultValue})`,
        );
      if (field.index) fieldBuilder.push(`index()`);
      if (field.required) fieldBuilder.push("required()");
      fields.push(`${fieldIndent}${key}: ${fieldBuilder.join(".")},`);
    }

    tableBuilder.push(`${tableIndent}${key}: model("${table.modelName}", {
${fields.join(",\n")}
${tableIndent}})`);
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
  file?: string,
): Promise<ClientAndSchema> {
  if (!file) return {};

  let contents = await fs.readFile(file, "utf-8");

  const result: ClientAndSchema = {};

  /**
   * Match schema:
   *   export const schema = defineSchema(...);
   *   const schema = defineSchema(...);
   *   export default defineSchema(...);
   */
  const schemaRegex =
    /(?:export\s+default\s+defineSchema\s*\(([\s\S]*?)\)\s*;)|(?:(export\s+)?(const|let|var)\s+(\w+)\s*=\s*defineSchema\s*\(([\s\S]*?)\)\s*;)/m;

  const schemaMatch = contents.match(schemaRegex);
  if (schemaMatch) {
    const fullMatch = schemaMatch[0];

    const isDefault = Boolean(schemaMatch[1]);
    const schemaContents = isDefault ? schemaMatch[1] : schemaMatch[5];
    const exportName = isDefault ? "default" : schemaMatch[4];

    result.schema = {
      default: isDefault,
      contents: schemaContents.trim(),
      exportName,
    };

    contents = contents.replace(fullMatch, "");
  }

  /**
   * Match client:
   *   export const client = Kineo(...);
   *   const client = Kineo(...);
   *   export default Kineo(...);
   */
  const clientRegex =
    /(?:export\s+default\s+Kineo\s*\([\s\S]*?\)\s*;)|(?:(export\s+)?(const|let|var)\s+\w+\s*=\s*Kineo\s*\([\s\S]*?\)\s*;)/m;

  const clientMatch = contents.match(clientRegex);
  if (clientMatch) {
    const [fullMatch] = clientMatch;

    result.client = fullMatch.trim();
    contents = contents.replace(fullMatch, "");
  }

  // Clean up excessive blank lines
  contents = contents.replace(/\n{3,}/g, "\n\n").trimEnd() + "\n";

  await fs.writeFile(file, contents, "utf-8");

  return result;
}
