import type { Adapter, MigrationEntry } from "@/adapter";
import { GraphModel } from "@/model";
import { compile } from "@/compilers/cypher";
import * as neo4j from "neo4j-driver";
import {
  field,
  FieldDef,
  relation,
  RelationDef,
  type ModelDef,
  type Schema,
} from "@/schema";

const META_LABEL = "`__MIGRATION$META__`";

/**
 * Authentication options for Neo4j.
 */
export type Auth =
  | {
      /**
       * Basic (username and password) authentication.
       */
      type: "basic";
      /**
       * The username.
       */
      username: string;
      /**
       * The password.
       */
      password: string;
      /**
       * An optional realm.
       */
      realm?: string;
    }
  | {
      /**
       * Bearer token authentication.
       */
      type: "bearer";
      /**
       * The token.
       */
      token: string;
    }
  | {
      /**
       * Kerberos ticket authentication.
       */
      type: "kerberos";
      /**
       * The ticket.
       */
      ticket: string;
    }
  | {
      /**
       * Customized authentication.
       */
      type: "custom";
      /**
       * The username.
       */
      principal: string;
      /**
       * The credentials.
       */
      credentials: string;
      /**
       * The realm of the authentication.
       */
      realm: string;
      /**
       * The scheme (`basic`, `bearer`, etc.)
       */
      scheme: string;
      /**
       * Optional parameters.
       */
      params: Record<string, any>;
    };

export type Neo4jOpts =
  | {
      /**
       * The driver.
       */
      driver: neo4j.Driver;
      /**
       * The session.
       */
      session?: neo4j.Session | neo4j.SessionConfig;
    }
  | {
      /**
       * The URL of your database.
       */
      url: string;
      /**
       * Authentication options.
       */
      auth: Auth;
      /**
       * Configuration for a Neo4j session.
       */
      session?: neo4j.SessionConfig;
    };

/**
 * A Neo4j adapter.
 */
export interface Neo4jAdapter
  extends Adapter<typeof GraphModel, neo4j.ResultSummary> {
  /**
   * The driver.
   */
  driver: neo4j.Driver;
  /**
   * The session.
   */
  session: neo4j.Session;
}

/**
 * Creates a new Neo4j adapter.
 * @param opts Options for creating the adapter.
 * @returns A Neo4j adapter.
 */
export function Neo4jAdapter(opts: Neo4jOpts): Neo4jAdapter {
  const driver =
    "driver" in opts ? opts.driver : neo4j.driver(opts.url, auth(opts.auth));
  const session =
    opts.session instanceof neo4j.Session
      ? opts.session
      : typeof opts.session === "undefined"
        ? driver.session()
        : driver.session(opts.session);
  return {
    Model: GraphModel,

    async close() {
      await session.close();
      await driver.close();
    },

    compile(ir) {
      return compile(ir);
    },

    async exec(result) {
      const { records, summary } = await session.run(
        result.command,
        result.params,
      );

      const entries: any[] = [];
      const edges: any[] = [];

      for (const record of records) {
        const obj: Record<string, any> = {};

        for (const key of record.keys) {
          const value = toNative(record.get(key));
          obj[key.toString()] = value;

          // Collect relationship-like objects
          collectEdges(value, edges);
        }

        entries.push(obj);
      }

      return {
        entries,
        entryCount: entries.length,
        edges,
        edgeCount: edges.length,
        summary,
        raw: records,
      };
    },

    driver,
    session,

    // KineoKit

    async pull() {
      const models: Schema = {};

      try {
        // 1. Get all labels in the DB
        const labelsRes = await session.run(`CALL db.labels()`);
        const labels = labelsRes.records.map((r) => r.get("label") as string);

        // Initialize models
        for (const label of labels) {
          models[label] = { $modelName: label };
        }

        // 2. Sample node properties from each label
        for (const label of labels) {
          const sampleRes = await session.run(
            `MATCH (n:\`${label}\`) RETURN n LIMIT 50`,
          );

          for (const record of sampleRes.records) {
            const node = record.get("n");
            mergeProperties(models[label], node.properties);
          }
        }

        // 3. Sample relationships for each label
        const relRes = await session.run(
          `
          MATCH (a)-[r]->(b)
          RETURN labels(a) AS fromLabels, type(r) AS relType, labels(b) AS toLabels
          LIMIT 1000
          `,
        );

        for (const row of relRes.records) {
          const fromLabels: string[] = row.get("fromLabels");
          const toLabels: string[] = row.get("toLabels");
          const relType: string = row.get("relType");

          // Neo4j nodes can have multiple labels, pick the first (or refine)
          const from = fromLabels[0];
          const to = toLabels[0];

          if (!from || !to) continue;

          mergeRelationship(models, from, relType, to, "outgoing");
          mergeRelationship(models, to, relType, from, "incoming");
        }
      } catch (err) {
        console.error("[kineo/neo4j] schema pulling error:", err);
      }

      return {
        schema: models,
        full: false,
      };
    },

    async push(schema: Schema) {
      // Wrap everything so one exception doesn't stop the rest
      async function tryRun(cypher: string) {
        try {
          await session.run(cypher);
        } catch (err: any) {
          // Neo4j will error with "already exists" -> ignore
          console.warn(
            "[kineo/neo4j] Skipped:",
            cypher,
            "Reason:",
            err.code || err.message,
          );
        }
      }

      // Convert schema key or $modelName to label
      function getLabel(name: string, model: ModelDef): string {
        return model.$modelName ?? name;
      }

      /**
       * For each model -> produce constraints & indexes
       */
      for (const [modelKey, modelDef] of Object.entries(schema)) {
        const label = getLabel(modelKey, modelDef);

        const fieldEntries = Object.entries(modelDef).filter(
          ([k, v]) => k !== "$modelName" && v instanceof FieldDef,
        ) as [string, FieldDef<any, any, any, any>][];

        const relationEntries = Object.entries(modelDef).filter(
          ([k, v]) => k !== "$modelName" && v instanceof RelationDef,
        ) as [string, RelationDef<any, any, any, any>][];

        // ------------------------------------------------------------
        // 1. FIELD-BASED NODE CONSTRAINTS
        // ------------------------------------------------------------

        for (const [propName, field] of fieldEntries) {
          const neoProp = field.rowName ?? propName;

          // 1a. ID -> unique constraint
          if (field.isId) {
            const cypher = `
          CREATE CONSTRAINT ${label}_${neoProp}_unique
          IF NOT EXISTS
          FOR (n:${label})
          REQUIRE n.${neoProp} IS UNIQUE
        `;
            await tryRun(cypher);
          }

          // 1b. Required -> existence constraint
          if (field.isRequired) {
            const cypher = `
          CREATE CONSTRAINT ${label}_${neoProp}_exists
          IF NOT EXISTS
          FOR (n:${label})
          REQUIRE n.${neoProp} IS NOT NULL
        `;
            await tryRun(cypher);
          }

          // 1c. Optional index (useful for search)
          if (!field.isId) {
            const cypher = `
          CREATE INDEX ${label}_${neoProp}_index
          IF NOT EXISTS
          FOR (n:${label})
          ON (n.${neoProp})
        `;
            await tryRun(cypher);
          }
        }

        // ------------------------------------------------------------
        // 2. RELATIONSHIP CONSTRAINTS
        // ------------------------------------------------------------

        for (const [relName, rel] of relationEntries) {
          const relLabel = rel.relLabel ?? relName;

          // Directions:
          // outgoing: (a)-[:REL]->(b)
          // incoming: (a)<-[:REL]-(b)
          // both:     (a)-[:REL]-(b)
          const direction = rel.relDirection;

          // Required relationship existence constraint
          if (rel.isRequired) {
            // For required rels we at least enforce presence of the relationship.
            // Neo4j supports relationship property constraints, but required relationships
            // must be enforced through pattern constraints (Neo4j 5+):
            //
            //   FOR (a:Label) REQUIRE (a)-[:REL]->() IS NOT EMPTY
            //
            let pattern = "";
            if (direction === "outgoing") {
              pattern = `(a:${label})-[:${relLabel}]->()`;
            } else if (direction === "incoming") {
              pattern = `(a:${label})<-[:${relLabel}]-()`;
            } else {
              pattern = `(a:${label})-[:${relLabel}]-()`;
            }

            const cypher = `
          CREATE CONSTRAINT ${label}_${relLabel}_rel_required
          IF NOT EXISTS
          FOR (a:${label})
          REQUIRE ${pattern} IS NOT EMPTY
        `;
            await tryRun(cypher);
          }
        }
      }

      console.log("[kineo/neo4j] Schema push completed.");
    },

    async deploy(migration, hash) {
      await session.run(migration);

      await session.run(
        `
        CREATE INDEX migration_meta_idx IF NOT EXISTS
        FOR (m:${META_LABEL}) ON (m.id)
        `,
      );

      await session.run(
        `
        MERGE (m:${META_LABEL} { id: $hash })
        SET m.deployed = true
        `,
        { hash },
      );
    },

    async status(_, hash) {
      const result = await session.run(
        `
        MATCH (m:${META_LABEL} { id: $hash })
        RETURN m.deployed AS deployed
        `,
        { hash },
      );

      if (result.records.length === 0) {
        return "pending";
      }

      const deployed = result.records[0].get("deployed");
      return deployed ? "completed" : "pending";
    },

    generate(prev, cur) {
      const migrations: MigrationEntry[] = [];

      const prevModels = new Set(Object.keys(prev || {}));
      const curModels = new Set(Object.keys(cur || {}));

      function findIdFieldName(modelDef: ModelDef): string | undefined {
        for (const k of Object.keys(modelDef)) {
          const v = (modelDef as any)[k];
          if (isFieldDef(v)) {
            if ((v as FieldDef<any, any, any, any>).isId) {
              return (v as FieldDef<any, any, any, any>).rowName || k;
            }
          }
        }
        return undefined;
      }

      // ---------- New models ----------
      for (const m of Object.keys(cur)) {
        if (!prevModels.has(m)) {
          const def = cur[m];
          const label = modelLabel(m, def);
          const idProp = findIdFieldName(def);

          if (idProp) {
            migrations.push({
              type: "command",
              description: `Create uniqueness constraint for new model ${label}`,
              command: `CREATE CONSTRAINT IF NOT EXISTS FOR (n:${label}) REQUIRE n.${idProp} IS UNIQUE;`,
              reverse: `DROP CONSTRAINT IF EXISTS FOR (n:${label}) REQUIRE n.${idProp} IS UNIQUE;`,
            });
          } else {
            migrations.push({
              type: "note",
              description: `New model ${label} added`,
              note: `Model '${label}' added. No id field found -> no constraint created automatically.`,
            });
          }
        }
      }

      // ---------- Removed models ----------
      for (const m of Object.keys(prev)) {
        if (!curModels.has(m)) {
          const def = prev[m];
          const label = modelLabel(m, def);
          const idProp = findIdFieldName(def);

          if (idProp) {
            migrations.push({
              type: "command",
              description: `Drop uniqueness constraint and delete nodes for removed model ${label}`,
              command:
                `DROP CONSTRAINT IF EXISTS FOR (n:${label}) REQUIRE n.${idProp} IS UNIQUE;\n` +
                `MATCH (n:${label}) DETACH DELETE n;`,
              reverse: "", // cannot bring deleted nodes back
            });
          } else {
            migrations.push({
              type: "command",
              description: `Delete nodes for removed model ${label}`,
              command: `MATCH (n:${label}) DETACH DELETE n;`,
              reverse: "", // irreversible
            });
          }
        }
      }

      // ---------- Existing models ----------
      for (const m of Object.keys(cur)) {
        if (!prev[m]) continue;

        const prevDef = prev[m];
        const curDef = cur[m];
        const label = modelLabel(m, curDef);

        const prevKeys = new Set(
          Object.keys(prevDef || {}).filter((k) => k !== "$modelName"),
        );
        const curKeys = new Set(
          Object.keys(curDef || {}).filter((k) => k !== "$modelName"),
        );

        // ---------- Added keys ----------
        for (const key of Array.from(curKeys)) {
          if (!prevKeys.has(key)) {
            const val = (curDef as any)[key];

            if (isFieldDef(val)) {
              const fieldDef = val as FieldDef<any, any, any, any>;
              const propName = fieldDef.rowName || key;

              if (fieldDef.defaultValue !== undefined) {
                migrations.push({
                  type: "command",
                  description: `Set default for added field ${propName} on ${label}`,
                  command: `MATCH (n:${label}) WHERE n.${propName} IS NULL OR NOT exists(n.${propName}) SET n.${propName} = ${serializeDefault(fieldDef.defaultValue)};`,
                  reverse: `MATCH (n:${label}) REMOVE n.${propName};`,
                });
              } else {
                migrations.push({
                  type: "note",
                  description: `Field ${propName} added to ${label}`,
                  note: `Field '${propName}' added with no default; existing nodes unchanged.`,
                });
              }

              if (fieldDef.isId) {
                migrations.push({
                  type: "command",
                  description: `Create uniqueness constraint for newly-added id field ${propName} on ${label}`,
                  command: `CREATE CONSTRAINT IF NOT EXISTS FOR (n:${label}) REQUIRE n.${propName} IS UNIQUE;`,
                  reverse: `DROP CONSTRAINT IF EXISTS FOR (n:${label}) REQUIRE n.${propName} IS UNIQUE;`,
                });
              }
            } else if (isRelationDef(val)) {
              const rel = val as RelationDef<any, any, any, any>;
              migrations.push({
                type: "note",
                description: `Relation ${key} added on ${label}`,
                note: `Relation '${key}' added on '${label}' pointing to '${rel.pointTo}'.`,
              });
            } else {
              migrations.push({
                type: "note",
                description: `Unknown key ${String(key)} added`,
                note: `Key '${String(key)}' added but not recognized.`,
              });
            }
          }
        }

        // ---------- Removed keys ----------
        for (const key of Array.from(prevKeys)) {
          if (!curKeys.has(key)) {
            const val = (prevDef as any)[key];

            if (isFieldDef(val)) {
              const fieldDef = val as FieldDef<any, any, any, any>;
              const propName = fieldDef.rowName || key;

              migrations.push({
                type: "command",
                description: `Remove property for removed field ${propName} on ${label}`,
                command: `MATCH (n:${label}) REMOVE n.${propName};`,
                reverse: "", // cannot restore old values
              });

              if (fieldDef.isId) {
                migrations.push({
                  type: "command",
                  description: `Drop uniqueness constraint for removed id field ${propName}`,
                  command: `DROP CONSTRAINT IF EXISTS FOR (n:${label}) REQUIRE n.${propName} IS UNIQUE;`,
                  reverse: `CREATE CONSTRAINT IF NOT EXISTS FOR (n:${label}) REQUIRE n.${propName} IS UNIQUE;`,
                });
              }
            } else if (isRelationDef(val)) {
              migrations.push({
                type: "note",
                description: `Relation ${key} removed from ${label}`,
                note: `Relation '${key}' removed; relationship data not automatically deleted.`,
              });
            } else {
              migrations.push({
                type: "note",
                description: `Unknown key ${String(key)} removed`,
                note: `Key '${String(key)}' removed from '${label}'.`,
              });
            }
          }
        }

        // ---------- Modified keys ----------
        for (const key of Array.from(curKeys)) {
          if (!prevKeys.has(key)) continue;

          const prevVal = (prevDef as any)[key];
          const curVal = (curDef as any)[key];

          if (isFieldDef(prevVal) && isFieldDef(curVal)) {
            const p = prevVal as FieldDef<any, any, any, any>;
            const c = curVal as FieldDef<any, any, any, any>;
            const propName = c.rowName || key;

            if (p.defaultValue !== c.defaultValue) {
              if (c.defaultValue !== undefined) {
                migrations.push({
                  type: "command",
                  description: `Apply new default for ${propName} on ${label}`,
                  command: `MATCH (n:${label}) WHERE n.${propName} IS NULL OR NOT exists(n.${propName}) SET n.${propName} = ${serializeDefault(c.defaultValue)};`,
                  reverse:
                    p.defaultValue !== undefined
                      ? `MATCH (n:${label}) WHERE n.${propName} = ${serializeDefault(c.defaultValue)} SET n.${propName} = ${serializeDefault(p.defaultValue)};`
                      : `MATCH (n:${label}) REMOVE n.${propName};`,
                });
              } else {
                migrations.push({
                  type: "note",
                  description: `Default removed for ${propName} on ${label}`,
                  note: `Default removed; no data change.`,
                });
              }
            }

            if (!p.isId && c.isId) {
              migrations.push({
                type: "command",
                description: `Create uniqueness constraint for ${propName}`,
                command: `CREATE CONSTRAINT IF NOT EXISTS FOR (n:${label}) REQUIRE n.${propName} IS UNIQUE;`,
                reverse: `DROP CONSTRAINT IF EXISTS FOR (n:${label}) REQUIRE n.${propName} IS UNIQUE;`,
              });
            }

            if (p.isId && !c.isId) {
              migrations.push({
                type: "command",
                description: `Drop uniqueness constraint for ${propName}`,
                command: `DROP CONSTRAINT IF EXISTS FOR (n:${label}) REQUIRE n.${propName} IS UNIQUE;`,
                reverse: `CREATE CONSTRAINT IF NOT EXISTS FOR (n:${label}) REQUIRE n.${propName} IS UNIQUE;`,
              });
            }

            if (p.kind !== c.kind) {
              migrations.push({
                type: "note",
                description: `Type changed for ${propName}`,
                note: `Type changed from '${p.kind}' to '${c.kind}'.`,
              });
            }

            if (p.isArray !== c.isArray) {
              migrations.push({
                type: "note",
                description: `Array flag changed for ${propName}`,
                note: `Array-ness changed; may require custom migration.`,
              });
            }
          } else if (isRelationDef(prevVal) && isRelationDef(curVal)) {
            const p = prevVal;
            const c = curVal;

            if (
              p.pointTo !== c.pointTo ||
              p.relLabel !== c.relLabel ||
              p.relDirection !== c.relDirection
            ) {
              migrations.push({
                type: "note",
                description: `Relation changed for '${key}'`,
                note: `Relation '${key}' changed; cannot auto-migrate data.`,
              });
            }
          } else {
            migrations.push({
              type: "note",
              description: `Key ${key} changed type`,
              note: `Field <-> relation change; no automatic migration.`,
            });
          }
        }
      }

      if (migrations.length === 0) {
        migrations.push({
          type: "note",
          description: "No detectable changes",
          note: "No differences between prev and cur schema.",
        });
      }

      return migrations;
    },
  };
}

/**
 * Produce a stable label for a model: prefer $modelName when set, otherwise use schema key.
 */
function modelLabel(key: string, def: any) {
  return (def && typeof def.$modelName === "string" && def.$modelName) || key;
}

function isFieldDef(v: any): v is FieldDef<any, any, any, any> {
  return v instanceof FieldDef;
}
function isRelationDef(v: any): v is RelationDef<any, any, any, any> {
  return v instanceof RelationDef;
}

/**
 * Serialize a default value into a Cypher literal.
 * - strings are quoted
 * - numbers/booleans emitted bare
 * - Dates serialized as datetime('...') (ISO)
 * - other values use a JSON.stringify fallback
 */
function serializeDefault(v: any): string {
  if (v === null) return "null";
  if (v === undefined) return "null";
  if (typeof v === "string") {
    // escape single quotes
    return `'${v.replace(/'/g, "\\'")}'`;
  }
  if (typeof v === "number" || typeof v === "boolean") {
    return String(v);
  }
  if (v instanceof Date) {
    return `datetime('${v.toISOString()}')`;
  }
  // fallback to json
  try {
    return JSON.stringify(v);
  } catch {
    return `'${String(v).replace(/'/g, "\\'")}'`;
  }
}

/**
 * Converts a Neo4j value to a vanilla JavaScript type.
 * @param value The value to convert.
 * @returns The converted value.
 */
function toNative(value: any): any {
  if (neo4j.isInt(value)) {
    // convert neo4j.Integer -> number (safe)
    return value.inSafeRange() ? value.toNumber() : value.toBigInt();
  }

  if (value instanceof neo4j.Node) {
    // Return node properties with an optional id & labels
    return {
      identity: toNative(value.identity),
      labels: value.labels,
      ...toNative(value.properties),
    };
  }

  if (value instanceof neo4j.Relationship) {
    // Return relationship properties with id, start, end
    return {
      identity: toNative(value.identity),
      start: toNative(value.start),
      end: toNative(value.end),
      type: value.type,
      ...toNative(value.properties),
    };
  }

  if (value instanceof neo4j.Path) {
    // Flatten paths to their nodes and relationships
    return {
      start: toNative(value.start),
      end: toNative(value.end),
      segments: value.segments.map((seg) => ({
        start: toNative(seg.start),
        relationship: toNative(seg.relationship),
        end: toNative(seg.end),
      })),
    };
  }

  if (Array.isArray(value)) {
    return value.map(toNative);
  }

  if (value && typeof value === "object") {
    const obj: Record<string, any> = {};
    for (const [k, v] of Object.entries(value)) obj[k] = toNative(v);
    return obj;
  }

  return value;
}

/**
 * Collects edges/relationship-like objects.
 * @param value The value.
 * @param edges The array to collect to.
 */
function collectEdges(value: any, edges: any[]) {
  if (!value) return;

  // Relationship-like object (produced by toNative)
  if (value.type && "start" in value && "end" in value && "identity" in value) {
    edges.push({
      id: value.identity,
      type: value.type,
      start: value.start,
      end: value.end,
      props: Object.fromEntries(
        Object.entries(value).filter(
          ([k]) => !["identity", "type", "start", "end"].includes(k),
        ),
      ),
    });
  }

  // Path-like object
  if (Array.isArray(value?.segments)) {
    for (const seg of value.segments) {
      collectEdges(seg.relationship, edges);
    }
  }

  // Recurse into arrays/objects
  if (Array.isArray(value)) {
    for (const v of value) collectEdges(v, edges);
  } else if (typeof value === "object") {
    for (const v of Object.values(value)) collectEdges(v, edges);
  }
}

/**
 * Creates an authentication token.
 * @param opts The authentication options.
 * @returns An authentication token.
 */
export function auth(opts: Auth): neo4j.AuthToken {
  switch (opts.type) {
    case "basic":
      return neo4j.auth.basic(opts.username, opts.password, opts.realm);
    case "bearer":
      return neo4j.auth.bearer(opts.token);
    case "kerberos":
      return neo4j.auth.kerberos(opts.ticket);
    case "custom":
      return neo4j.auth.custom(
        opts.principal,
        opts.credentials,
        opts.realm,
        opts.scheme,
        opts.params,
      );
  }
}

// KineoKit utilities

/**
 * Infer field type from Neo4j value.
 */
function inferKind(value: any): FieldDef<any, any, any, any> {
  if (Array.isArray(value)) {
    // recursively infer base type
    if (value.length === 0) return field.string().array(); // unknown empty array
    const base = inferKind(value[0]);
    return base.array();
  }
  if (typeof value === "string") return field.string();
  if (typeof value === "number") return field.float(); // could refine via integer check
  if (typeof value === "boolean") return field.bool();
  if (neo4j.isInt(value)) return field.int();
  if (value instanceof Date) return field.datetime();
  return field.string(); // fallback
}

/**
 * Merges new inferred fields into a model definition.
 */
function mergeProperties(model: any, props: Record<string, any>) {
  for (const [key, value] of Object.entries(props)) {
    if (!model[key]) {
      model[key] = inferKind(value).name(key);
    }
  }
}

/**
 * Adds or merges a relationship.
 */
function mergeRelationship(
  models: Record<string, any>,
  from: string,
  relType: string,
  to: string,
  direction: "outgoing" | "incoming",
) {
  const model = models[from] ?? (models[from] = { $modelName: from });

  if (!model[relType]) {
    model[relType] = relation.to(to, relType).direction(direction);
  } else {
    // already exists: update direction heuristically
    const rel: RelationDef<any> = model[relType];
    if (rel.relDirection !== direction) {
      rel.both(relType);
    }
  }
}
