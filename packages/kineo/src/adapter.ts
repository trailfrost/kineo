import type { IR } from "./ir";
import type { Schema, SchemaDiff } from "./schema";

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
   * Gets schema from database.
   */
  getSchema(): OptPromise<Schema>;

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
  push(schema: Schema): OptPromise<void>;
  /**
   * Generates migrations for the database.
   */
  migrate(diff: SchemaDiff): OptPromise<string[]>;
  /**
   * Gets the status for all migrations.
   */
  status(
    migrations: string[],
    hashes: string[],
  ): OptPromise<Array<"deployed" | "pending">>;

  /**
   * Deploys a series of migrations to the database.
   * @param migrations The migrations queries to deploy.
   */
  deploy(migration: string, hash: string): OptPromise<void>;
};

export type CompileResult = {
  command: string;
  params: Record<string, unknown>;
};
export type Compiler<TDialect = undefined> = TDialect extends
  | null
  | undefined
  | never
  ? (ir: IR) => CompileResult
  : (ir: IR, dialect: TDialect) => CompileResult;

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
  key: string | number,
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
  prop: string,
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
    elementId: string,
  ) {
    this.identity = identity;
    this.labels = labels;
    this.properties = properties;
    this.elementId = elementId;
  }
}
