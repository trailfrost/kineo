import { getNodeProp, getScalar, type Adapter } from "../../adapter";
import {
  field,
  FieldDef,
  Node,
  relation,
  RelationshipDef,
  type Schema,
} from "../../schema";
import compile from "../../compilers/cypher";
import GraphModel from "../../graph/model";
import * as neo4j from "neo4j-driver";
import {
  parseGetNodeLabels,
  parseGetNodeProperties,
  parseGetRelationshipProperties,
  parseGetRelationshipTypes,
} from "./ir";

/**
 * Neo4j authentication options.
 */
export type Auth =
  | {
      type?: "basic";
      username: string;
      password: string;
      realm?: string;
    }
  | {
      type: "bearer";
      token: string;
    }
  | {
      type: "kerberos";
      ticket: string;
    }
  | {
      type: "custom";
      principal: string;
      credentials: string;
      realm: string;
      scheme: string;
      parameters: Record<string, unknown>;
    };

export type AdapterConfig =
  | ({
      url: string;
      auth: Auth;
    } & neo4j.Config)
  | {
      driver: neo4j.Driver;
    };

/**
 * Creates a new Neo4j driver.
 * @param url URL to your database.
 * @param auth Authentication options.
 * @param config Driver configuration.
 * @returns A Neo4j driver.
 */
function createDriver(config: AdapterConfig): neo4j.Driver {
  if ("driver" in config) return config.driver;

  let authToken: neo4j.AuthToken;

  switch (config.auth.type) {
    case "basic":
    case undefined:
      authToken = neo4j.auth.basic(config.auth.username, config.auth.password);
      break;
    case "bearer":
      authToken = neo4j.auth.bearer(config.auth.token);
      break;
    case "kerberos":
      authToken = neo4j.auth.kerberos(config.auth.ticket);
      break;
    case "custom":
      authToken = neo4j.auth.custom(
        config.auth.principal,
        config.auth.credentials,
        config.auth.realm,
        config.auth.scheme,
        config.auth.parameters
      );
      break;
  }

  return neo4j.driver(config.url, authToken, config);
}

/**
 * Type for Kineo Neo4j adapter.
 */
export type Kineo4j = Adapter & {
  driver: neo4j.Driver;
  session: neo4j.Session;
};

export class Neo4jModel<
  S extends Schema,
  N extends Node,
  A extends Adapter,
> extends GraphModel<S, N, A> {
  /**
   * Gets all labels (graph database specific).
   * @returns Labels.
   */
  async getNodeLabels(): Promise<string[]> {
    const ir = parseGetNodeLabels(this.label, "l");
    const res = await this.run(ir);
    const row = res.records[0];
    return (
      getScalar<string[]>(row, "l") ?? getNodeProp<string[]>(row, 0, "l") ?? []
    );
  }

  /**
   * Gets all relationship types (graph database specific).
   * @returns Types.
   */
  async getRelationshipTypes(): Promise<string[]> {
    const ir = parseGetRelationshipTypes(this.label, "t");
    const res = await this.run(ir);
    const row = res.records[0];
    return (
      getScalar<string[]>(row, "t") ?? getNodeProp<string[]>(row, 0, "t") ?? []
    );
  }

  /**
   * Gets properties of a node (graph database specific).
   * @param label The label to get properties of.
   * @param arrayType Only enable this if node types are arrays.
   * @returns Node properties.
   */
  async getNodeProperties(): Promise<string[]> {
    const ir = parseGetNodeProperties(this.label, "p");
    const res = await this.run(ir);
    const row = res.records[0];
    return (
      getScalar<string[]>(row, "p") ?? getNodeProp<string[]>(row, 0, "p") ?? []
    );
  }

  /**
   * Gets relationship properties (graph database specific).
   * @param type The relationship type.
   * @returns Relationship properties.
   */
  async getRelationshipProperties(type: string): Promise<string[]> {
    const ir = parseGetRelationshipProperties(this.label, "p", type);
    const res = await this.run(ir);
    const row = res.records[0];
    return (
      getScalar<string[]>(row, "p") ?? getNodeProp<string[]>(row, 0, "p") ?? []
    );
  }
}

/**
 * Creates a Neo4j adapter for Kineo.
 * @param config The configuration for the adapter.
 * @returns A Neo4j adapter.
 */
export function Neo4jAdapter(config: AdapterConfig): Kineo4j {
  const driver = createDriver(config);
  const session = driver.session();

  return {
    driver,
    session,

    Model: Neo4jModel,

    compile,

    async getSchema(): Promise<Schema> {
      const session = driver.session();

      // Get node properties
      const nodePropsResult = await session.run(
        "CALL db.schema.nodeTypeProperties()"
      );

      // Get rel properties
      // const relPropsResult = await session.run(
      //   "CALL db.schema.relTypeProperties()"
      // );
      // ? Not needed for now

      // Get relationships (for directions)
      const relTypesResult = await session.run(
        "CALL db.schema.visualization()"
      );

      // Process node properties into a Kineo Schema
      const schema: Schema = {};

      for (const record of nodePropsResult.records) {
        const label = record.get("nodeLabels")[0] as string; // e.g. "User"
        const propName = record.get("propertyName") as string;
        const propType = record.get("propertyTypes")[0] as string;
        const mandatory = record.get("mandatory") as boolean;

        if (!schema[label]) schema[label] = {};

        schema[label][propName] = mandatory
          ? fieldFromNeo4jType(propType).required()
          : fieldFromNeo4jType(propType).optional();
      }

      // Add relationships
      for (const record of relTypesResult.records) {
        const nodes = record.get("nodes");
        const rels = record.get("relationships");

        for (const rel of rels) {
          const start = nodes.find(
            (n: { identity: { equals(other: unknown): boolean } }) =>
              n.identity.equals(rel.startNode)
          );
          const end = nodes.find(
            (n: { identity: { equals(other: unknown): boolean } }) =>
              n.identity.equals(rel.endNode)
          );
          const startLabel = start.labels[0];
          const endLabel = end.labels[0];
          const type = rel.type;

          if (!schema[startLabel]) schema[startLabel] = {};

          schema[startLabel][type] = relation
            .to(endLabel)
            .outgoing(type)
            .required();
        }
      }

      return schema;
    },

    async close() {
      await session.close();
      await driver.close();
    },

    async run(command, params) {
      const result = await session.run(command, params);
      return {
        records: result.records.map((record) => record.get(0)),
      };
    },

    async push(schema) {
      console.warn("[warning] Neo4j doesn't enforce a schema. Trying my best.");
      for (const [nodeName, nodeDef] of Object.entries(schema)) {
        for (const [fieldName, def] of Object.entries(nodeDef)) {
          if (def instanceof FieldDef) {
            // Primary key
            if (def.isPrimaryKey) {
              await session.run(
                `CREATE CONSTRAINT ${nodeName}_${fieldName}_pk IF NOT EXISTS FOR (n:${nodeName}) REQUIRE n.${fieldName} IS UNIQUE`
              );
            }

            // Required (existence constraint)
            if (def.isRequired) {
              await session.run(
                `CREATE CONSTRAINT ${nodeName}_${fieldName}_exists IF NOT EXISTS FOR (n:${nodeName}) REQUIRE n.${fieldName} IS NOT NULL`
              );
            }

            // Uniqueness constraint (if not primary key already)
            if (def.isUnique && !def.isPrimaryKey) {
              await session.run(
                `CREATE CONSTRAINT ${nodeName}_${fieldName}_unique IF NOT EXISTS FOR (n:${nodeName}) REQUIRE n.${fieldName} IS UNIQUE`
              );
            }
          }

          // Neo4j doesn't enforce schema for relationships
          if (def instanceof RelationshipDef) {
            // Optional: create index for relationship metadata
            if (def.metadata && Object.keys(def.metadata).length > 0) {
              for (const metaKey of Object.keys(def.metadata)) {
                await session.run(
                  `CREATE INDEX ${nodeName}_${fieldName}_${metaKey}_idx IF NOT EXISTS FOR ()-[r:${def.refLabel}]-() ON (r.${metaKey})`
                );
              }
            }
          }
        }
      }
    },

    async status() {
      console.error(
        "[fatal] Neo4j doesn't enforce a schema, therefore migrations can't be generated."
      );
      return [];
    },

    async deploy() {
      console.error(
        "[fatal] Neo4j doesn't enforce a schema, therefore migrations can't be deployed."
      );
    },

    async migrate() {
      console.error(
        "[fatal] Neo4j doesn't enforce a schema, therefore migrations can't be generated."
      );
      return [];
    },
  };
}

function fieldFromNeo4jType(neoType: string) {
  switch (neoType) {
    case "String":
      return field.string();
    case "Integer":
      return field.integer();
    case "Float":
      return field.float();
    case "Boolean":
      return field.boolean();
    case "Date":
      return field.date();
    case "LocalDateTime":
      return field.localDatetime();
    case "DateTime":
      return field.zonedDatetime();
    case "Time":
      return field.zonedTime();
    case "LocalTime":
      return field.localTime();
    case "Duration":
      return field.duration();
    case "Point":
      return field.point();
    case "Map":
      return field.map();
    default:
      return field.any();
  }
}
