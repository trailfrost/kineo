import fs from "fs/promises";
import { Project, SyntaxKind, Node } from "ts-morph";
import type { BetterAuthDBSchema } from "better-auth/db";

export async function createSchema({
  tables,
  file = "./src/db.ts",
}: {
  tables: BetterAuthDBSchema;
  file?: string;
}) {
  const tableBuilder: string[] = [];
  const tableIndent = " ".repeat(2);

  for (const tableKey in tables) {
    const table = tables[tableKey];
    const fields: string[] = [];
    const fieldIndent = " ".repeat(4);

    for (const fieldKey in table.fields) {
      const field = table.fields[fieldKey];
      const fieldBuilder: string[] = [];

      if (field.references) {
        fieldBuilder.push(`relation.to("${field.references.model}")`);
      } else {
        fieldBuilder.push(
          `field.${
            typeof field.type === "string"
              ? field.type.endsWith("[]")
                ? `${field.type}(${field.fieldName ? `"${field.fieldName}"` : ""}).array()`
                : `${field.type}(${field.fieldName ? `"${field.fieldName}"` : ""})`
              : `string(${field.fieldName ? `"${field.fieldName}"` : ""}).array()`
          }`
        );
      }

      if (field.defaultValue) {
        fieldBuilder.push(
          `default(${
            typeof field.defaultValue === "function"
              ? field.defaultValue()
              : field.defaultValue
          })`
        );
      }
      if (field.index) fieldBuilder.push("index()");
      if (field.required) fieldBuilder.push("required()");

      fields.push(`${fieldIndent}${fieldKey}: ${fieldBuilder.join(".")},`);
    }

    tableBuilder.push(
      `${tableIndent}${tableKey}: model("${table.modelName}", {\n${fields.join(
        "\n"
      )}\n${tableIndent}}),`
    );
  }

  await replaceSchemaInPlace(file, tableBuilder.join("\n\n"));

  return {
    path: file,
    code: "",
  };
}

export async function replaceSchemaInPlace(file: string, newTables: string) {
  const project = new Project({
    useInMemoryFileSystem: true,
    skipAddingFilesFromTsConfig: true,
  });

  const contents = await fs.readFile(file, "utf-8");
  const sourceFile = project.createSourceFile(file, contents, {
    overwrite: true,
  });

  // step 1: collect all local identifiers that resolve to `defineSchema`
  const defineSchemaNames = new Set<string>();

  for (const decl of sourceFile.getImportDeclarations()) {
    const named = decl.getNamedImports();
    for (const spec of named) {
      const imported = spec.getName();
      if (imported === "defineSchema") {
        defineSchemaNames.add(spec.getAliasNode()?.getText() ?? imported);
      }
    }
  }

  // handle `const defineSchema = ...` edge cases
  defineSchemaNames.add("defineSchema");

  // step 2: find the call expression
  const callExpr = sourceFile
    .getDescendantsOfKind(SyntaxKind.CallExpression)
    .find((call) => {
      const expr = call.getExpression();
      if (!Node.isIdentifier(expr)) return false;
      return defineSchemaNames.has(expr.getText());
    });

  if (!callExpr) {
    return;
  }

  // step 3: replace the object literal argument
  const args = callExpr.getArguments();
  if (args.length === 0) {
    return;
  }

  args[0].replaceWithText(`{\n${newTables}\n}`);

  // step 4: write back file
  await fs.writeFile(file, sourceFile.getFullText(), "utf-8");
}
