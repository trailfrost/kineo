import type { IR } from "./ir";
import type { Schema } from "./schema";

/**
 * A diff between two schemas.
 *
 * This only describes the structure of a diff,
 * not the actual "before" and "after" values.
 */
export type SchemaDiff = {
  /** Added nodes keyed by name */
  addedNodes?: Record<string, NodeDiff>;
  /** Removed nodes keyed by name */
  removedNodes?: Record<string, NodeDiff>;
  /** Nodes that exist in both schemas but differ */
  changedNodes?: Record<string, NodeChangeDiff>;
};

/**
 * A diff at the node level.
 */
export type NodeDiff = {
  /** Added fields */
  addedFields?: Record<string, FieldDiff | RelationshipDiff>;
  /** Removed fields */
  removedFields?: Record<string, FieldDiff | RelationshipDiff>;
  /** Changed fields */
  changedFields?: Record<string, FieldChangeDiff | RelationshipChangeDiff>;
};

/**
 * A diff for a field.
 */
export type FieldDiff = {
  kind: "FIELD";
};

/**
 * A diff for a relationship.
 */
export type RelationshipDiff = {
  kind: "RELATIONSHIP";
};

/**
 * A diff representing changes in an existing field.
 */
export type FieldChangeDiff = {
  kind: "FIELD";
  /** Flags for what aspect of the field changed */
  typeChanged?: true;
  requiredChanged?: true;
  arrayChanged?: true;
  idChanged?: true;
  uniqueChanged?: true;
  defaultChanged?: true;
};

/**
 * A diff representing changes in an existing relationship.
 */
export type RelationshipChangeDiff = {
  kind: "RELATIONSHIP";
  /** Flags for what aspect of the relationship changed */
  toChanged?: true;
  labelChanged?: true;
  directionChanged?: true;
  requiredChanged?: true;
  arrayChanged?: true;
  defaultChanged?: true;
  metadataChanged?: true;
};

/**
 * A diff for a node that existed in both schemas
 * but has internal changes.
 */
export type NodeChangeDiff = NodeDiff & {
  /** Node name that changed */
  name: string;
};

/**
 * Parameters. Used for passing parameters into queries.
 */
export type Params = Record<string, unknown>;

/**
 * A command.
 */
export type Command = { command: string; params: Params };

/**
 * All supported scalar types.
 */
export type Scalar =
  | null
  | boolean
  | number
  | bigint
  | string
  | string[]
  | number[]
  | boolean[];

/**
 * A node or a scalar type.
 */
export type Cell = Node | Scalar;

/**
 * A query record. Contains all return values for a command.
 */
export type QueryRecord = Map<number | string, Cell>;

/**
 * The result of a query.
 */
export type QueryResult = {
  records: QueryRecord[];
};

/**
 * Either a `Promise` or not.
 */
export type OptPromise<T> = T | Promise<T>;

/**
 * Main adapter type.
 */
export type Adapter = {
  /**
   * What types of schema introspection your database supports.
   */
  schemaIntrospection: Array<
    | "model_list" // List all models (tables, node labels, collections)
    | "system_model_list" // List of system/internal models to ignore
    | "field_list" // List of fields/properties for a given model
    | "field_type" // Field data type (including precision, scale)
    | "field_nullability" // Whether field is nullable
    | "field_default" // Default values (static or computed)
    | "field_auto_increment" // Auto-increment / identity info
    | "field_array" // Whether a field is an array/multi-valued
    | "field_computed" // Computed/generated columns
    | "field_collation" // Collation/charset info
    | "field_check_constraints" // Check constraints (e.g., value ranges)
    | "field_enum_values" // Enum allowed values
    | "primary_key" // Primary key(s) for a model
    | "unique_constraints" // Unique constraints on one or more fields
    | "foreign_keys" // Foreign key constraints
    | "foreign_key_actions" // FK update/delete actions (cascade, restrict, etc.)
    | "indexes" // Index definitions (basic)
    | "index_details" // Index details (order, partial, expressions)
    | "relations" // Relationship definitions (graph DB: edge types)
    | "relation_properties" // Properties/fields on relationships
    | "relation_multiplicity" // Min/max cardinality if supported
    | "triggers" // Trigger definitions
    | "functions" // Functions/procedures tied to schema
    | "materialized_views" // Materialized views
    | "views" // Views
    | "exclusion_constraints" // Exclusion constraints (Postgres, etc.)
  >;

  /**
   * Gets schema from database.
   */
  getSchema(): Promise<Schema>;

  /**
   * Close the adapter.
   */
  close(): OptPromise<void>;
  /**
   * Compile an intermediate representation to your query language.
   * @param ir The IR to compile.
   */
  compile(ir: IR): OptPromise<Command>;
  /**
   * Runs a command.
   * @param command The command to run.
   * @param params Parameters to that command.
   */
  run(command: string, params: Params): OptPromise<QueryResult>;

  /**
   * Pushes a schema to the database. You don't need to warn the user, Kineo already does that for you.
   */
  push(): OptPromise<void>;
  /**
   * Generates migrations for the database.
   */
  migrate(): OptPromise<string[]>;
  /**
   * Gets the status for all migrations.
   */
  status(migrations: string[]): OptPromise<Array<"deployed" | "pending">>;
  /**
   * Deploys a series of migrations to the database.
   * @param migrations The migrations queries to deploy.
   */
  deploy(migrations: string[]): OptPromise<void>;
};

/**
 * Checks if a value is a node.
 * @param v The value to check.
 * @returns `true` is the value is a node.
 */
export const isNode = (v: unknown): v is Node => v instanceof Node;

/**
 * Gets a scalar (primitive) node.
 * @param rec The record to check against.
 * @param key The key to get.
 * @returns Undefined if the value is a node, otherwise a plain value if it exists.
 */
export const getScalar = <T extends Scalar>(
  rec: QueryRecord | undefined,
  key: string | number
): T | undefined => {
  const v = rec?.get?.(key);
  return isNode(v) ? undefined : (v as T | undefined);
};

/**
 * Gets property from a node.
 * @param rec The record to get from.
 * @param idx The index to get.
 * @param prop The property name.
 * @returns The property or undefined if it doesn't exist.
 */
export const getNodeProp = <T>(
  rec: QueryRecord | undefined,
  idx: number | string,
  prop: string
): T | undefined => {
  const v = rec?.get?.(idx);
  return isNode(v) ? (v.properties[prop] as T | undefined) : undefined;
};

/**
 * A node. Could also represent a model.
 */
export class Node {
  /**
   * Node identity.
   */
  identity: number | bigint;
  /**
   * Node labels.
   */
  labels: string[];
  /**
   * Node properties.
   */
  properties: Params;
  /**
   * Node element identifier.
   */
  elementId: string;

  /**
   * Creates a new node.
   * @param identity Node identity.
   * @param labels Node labels.
   * @param properties Node properties.
   * @param elementId Node element ID.
   */
  constructor(
    identity: number | bigint,
    labels: string[],
    properties: Params,
    elementId: string
  ) {
    this.identity = identity;
    this.labels = labels;
    this.properties = properties;
    this.elementId = elementId;
  }
}
