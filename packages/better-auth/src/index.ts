import type { Kineo } from "kineo";
import { createAdapterFactory } from "better-auth/adapters";
import fs from "fs/promises";

export const kineoAdapter = (client: Kineo<any, any>) =>
  createAdapterFactory({
    config: {
      adapterId: "@kineojs/better-auth",
    },
    adapter: () => ({
      // TODO

      async createSchema({ tables, file }) {
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
      },
    }),
  });

interface ClientAndSchema {
  schema?: {
    default: boolean;
    contents: string;
    exportName: string;
  };
  client?: string;
}

async function deleteClientAndSchema(file?: string): Promise<ClientAndSchema> {
  if (!file) return {};

  // TODO
  return {};
}
