import type { Adapter } from "kineo/adapter";
import type {
  FieldDiff,
  NodeDiff,
  RelationshipDiff,
  SchemaDiff,
  Schema,
} from "kineo/schema";
import { FieldDef, RelationshipDef } from "kineo/schema";

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

export async function push(adapter: Adapter, schema: Schema) {
  return await adapter.push(schema);
}

export async function schemaDiff(
  adapter: Adapter,
  prev: Schema
): Promise<SchemaDiff> {
  const current = await adapter.getSchema();
  const nodeDiffs: NodeDiff[] = [];

  // Find removed + changed nodes
  for (const nodeName of Object.keys(prev)) {
    const prevNode = prev[nodeName];
    const currNode = current[nodeName];

    if (!currNode) {
      nodeDiffs.push({
        kind: "node.removed",
        severity: "breaking",
        node: prevNode,
      });
      continue;
    }

    // Compare fields + relationships
    const fieldDiffs: FieldDiff[] = [];
    const relationshipDiffs: RelationshipDiff[] = [];

    // Field removals & changes
    for (const fieldName of Object.keys(prevNode)) {
      const prevField = prevNode[fieldName];
      const currField = currNode[fieldName];

      if (!currField) {
        if (prevField instanceof FieldDef) {
          fieldDiffs.push({
            kind: "field.removed",
            severity: "breaking",
            field: prevField,
          });
        } else if (prevField instanceof RelationshipDef) {
          relationshipDiffs.push({
            kind: "relationship.removed",
            severity: "breaking",
            relationship: prevField,
          });
        }
        continue;
      }

      // Both exist → compare properties
      if (prevField instanceof FieldDef && currField instanceof FieldDef) {
        if (prevField.fieldType !== currField.fieldType) {
          fieldDiffs.push({
            kind: "field.changedType",
            severity: "breaking",
            from: prevField,
            to: currField,
          });
        }
        if (prevField.isRequired !== currField.isRequired) {
          fieldDiffs.push({
            kind: "field.changedRequired",
            severity:
              currField.isRequired && !prevField.isRequired
                ? "breaking"
                : "non-breaking",
            from: prevField,
            to: currField,
          });
        }
        if (prevField.isArray !== currField.isArray) {
          fieldDiffs.push({
            kind: "field.changedArray",
            severity: "breaking",
            from: prevField,
            to: currField,
          });
        }
      } else if (
        prevField instanceof RelationshipDef &&
        currField instanceof RelationshipDef
      ) {
        if (prevField.refDirection !== currField.refDirection) {
          relationshipDiffs.push({
            kind: "relationship.changedDirection",
            severity: "breaking",
            from: prevField,
            to: currField,
          });
        }
        if (prevField.isRequired !== currField.isRequired) {
          relationshipDiffs.push({
            kind: "relationship.changedRequired",
            severity:
              currField.isRequired && !prevField.isRequired
                ? "breaking"
                : "non-breaking",
            from: prevField,
            to: currField,
          });
        }
        if (prevField.isArray !== currField.isArray) {
          relationshipDiffs.push({
            kind: "relationship.changedArray",
            severity: "breaking",
            from: prevField,
            to: currField,
          });
        }
      }
    }

    // Field additions
    for (const fieldName of Object.keys(currNode)) {
      if (!(fieldName in prevNode)) {
        const currField = currNode[fieldName];
        if (currField instanceof FieldDef) {
          fieldDiffs.push({
            kind: "field.added",
            severity: "non-breaking",
            field: currField,
          });
        } else if (currField instanceof RelationshipDef) {
          relationshipDiffs.push({
            kind: "relationship.added",
            severity: "non-breaking",
            relationship: currField,
          });
        }
      }
    }

    if (fieldDiffs.length > 0 || relationshipDiffs.length > 0) {
      const severity = [...fieldDiffs, ...relationshipDiffs].some(
        (d) => d.severity === "breaking"
      )
        ? "breaking"
        : "non-breaking";

      nodeDiffs.push({
        kind: "node.changed",
        severity,
        nodeName,
        fieldDiffs,
        relationshipDiffs,
      });
    }
  }

  // Find added nodes
  for (const nodeName of Object.keys(current)) {
    if (!(nodeName in prev)) {
      nodeDiffs.push({
        kind: "node.added",
        severity: "non-breaking",
        node: current[nodeName],
      });
    }
  }

  const severity = nodeDiffs.some((d) => d.severity === "breaking")
    ? "breaking"
    : "non-breaking";

  return {
    kind: "schema.diff",
    severity,
    nodeDiffs,
  };
}

export async function migrate(
  adapter: Adapter,
  diff: SchemaDiff
): Promise<string[]> {
  return await adapter.migrate(diff);
}

export async function status(
  adapter: Adapter,
  migrations: string[],
  hashes: string[]
): Promise<Array<"pending" | "deployed">> {
  return await adapter.status(migrations, hashes);
}

export async function deploy(
  adapter: Adapter,
  migration: string,
  hash: string
) {
  return await adapter.deploy(migration, hash);
}
