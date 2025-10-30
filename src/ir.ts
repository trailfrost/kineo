import type * as model from "./model";

export enum StatementType {
  Find = "Find",
  Count = "Count",
  Create = "Create",
  Upsert = "Upsert",
  Delete = "Delete",
  ConnectQuery = "ConnectQuery",
  RelationQuery = "RelationQuery",
}

/**
 * Common base statement
 */
export interface Statement {
  type: StatementType;
  model: string; // model name
  label?: string; // optional label for the statement (for debugging)
  alias?: string; // alias for the result
}

/**
 * IR root — represents a compiled set of model operations
 */
export interface IR {
  statements: Statement[];
}

// ---------- Specialized IR Statement Definitions ---------- //

/**
 * A `findFirst`/`findMany` statement.
 */
export interface FindStatement extends Statement {
  type: StatementType.Find;
  where?: Record<string, any>;
  select?: Record<string, any>;
  include?: Record<string, any>;
  orderBy?: Record<string, "asc" | "desc">[];
  distinct?: string[];
  skip?: number;
  take?: number;
}

/**
 * A `count` statement.
 */
export interface CountStatement extends Statement {
  type: StatementType.Count;
  where?: Record<string, any>;
}

/**
 * A `create` statement.
 */
export interface CreateStatement extends Statement {
  type: StatementType.Create;
  data: Record<string, any>;
  select?: Record<string, any>;
  include?: Record<string, any>;
}

/**
 * An `update`/`updateMany` statement.
 */
export interface UpdateStatement extends Statement {
  type: StatementType.Upsert;
  where: Record<string, any>;
  data: Record<string, any>;
  select?: Record<string, any>;
  include?: Record<string, any>;
}

/**
 * A `delete`/`deleteMany` statement.
 */
export interface DeleteStatement extends Statement {
  type: StatementType.Delete;
  where: Record<string, any>;
}

/**
 * A `connect`/`disconnect` statement.
 */
export interface ConnectQueryStatement extends Statement {
  type: StatementType.ConnectQuery;
  from: Record<string, any>;
  to: Record<string, any>;
  relation: string;
  direction?: string;
  properties?: Record<string, any>;
}

/**
 * A `findPath`/`findShortestPath`/`findAllPaths` query.
 */
export interface RelationQueryStatement extends Statement {
  type: StatementType.RelationQuery;
  from: Record<string, any>;
  to: Record<string, any>;
  maxDepth?: number;
  minDepth?: number;
  direction?: string;
  limit?: number;
}

// ---------- Parser / Compiler Utilities ---------- //

/**
 * Compiles a `QueryOpts` into a `FindStatement`
 */
export function compileFindStatement(
  modelName: string,
  opts: model.QueryOpts<any, any>
): FindStatement {
  return {
    type: StatementType.Find,
    model: modelName,
    where: opts.where,
    select: opts.select,
    include: opts.include,
    orderBy: opts.orderBy as any,
    distinct: opts.distinct ? opts.distinct.map(String) : undefined,
    skip: opts.skip,
    take: opts.take,
  };
}

/**
 * Compiles a `Count` query
 */
export function compileCountStatement(
  modelName: string,
  opts: model.QueryOpts<any, any>
): CountStatement {
  return {
    type: StatementType.Count,
    model: modelName,
    where: opts.where,
  };
}

/**
 * Compiles a `Create` query
 */
export function compileCreateStatement(
  modelName: string,
  opts: model.CreateOpts<any, any>
): CreateStatement {
  return {
    type: StatementType.Create,
    model: modelName,
    data: opts.data,
    select: opts.select,
    include: opts.include,
  };
}

/**
 * Compiles an `Update` or `Upsert` query
 */
export function compileUpsertStatement(
  modelName: string,
  opts: model.UpsertOpts<any, any>
): UpdateStatement {
  return {
    type: StatementType.Upsert,
    model: modelName,
    where: opts.where,
    data: {
      create: opts.create,
      update: opts.update,
    },
    select: opts.select,
    include: opts.include,
  };
}

/**
 * Compiles a `Delete` query
 */
export function compileDeleteStatement(
  modelName: string,
  opts: model.DeleteOpts<any, any>
): DeleteStatement {
  return {
    type: StatementType.Delete,
    model: modelName,
    where: opts.where,
  };
}

/**
 * Compiles a `Connect` query
 */
export function compileConnectQueryStatement(
  modelName: string,
  opts: model.ConnectOpts<any, any>
): ConnectQueryStatement {
  return {
    type: StatementType.ConnectQuery,
    model: modelName,
    from: opts.from.where,
    to: opts.to.where,
    relation: opts.relation,
    direction: opts.direction,
    properties: opts.properties,
  };
}

/**
 * Compiles a `Path` / Relation traversal query
 */
export function compileRelationQueryStatement(
  modelName: string,
  opts: model.PathOpts<any, any>
): RelationQueryStatement {
  return {
    type: StatementType.RelationQuery,
    model: modelName,
    from: opts.from.where,
    to: opts.to.where,
    maxDepth: opts.maxDepth,
    minDepth: opts.minDepth,
    direction: opts.direction,
    limit: opts.limit,
  };
}

// ---------- IR Construction Helpers ---------- //

/**
 * Takes one or more parsed statements and wraps them in an IR container.
 */
export function makeIR(...statements: Statement[]): IR {
  return { statements };
}

/**
 * Convenience: compile a generic model operation into IR
 */
export function compileToIR(modelName: string, op: string, opts: any): IR {
  let stmt: Statement;

  switch (op) {
    case "findFirst":
    case "findMany":
      stmt = compileFindStatement(modelName, opts);
      break;
    case "count":
      stmt = compileCountStatement(modelName, opts);
      break;
    case "create":
    case "createMany":
      stmt = compileCreateStatement(modelName, opts);
      break;
    case "upsert":
    case "upsertMany":
      stmt = compileUpsertStatement(modelName, opts);
      break;
    case "delete":
    case "deleteMany":
      stmt = compileDeleteStatement(modelName, opts);
      break;
    case "connect":
    case "disconnect":
      stmt = compileConnectQueryStatement(modelName, opts);
      break;
    case "findPath":
    case "findShortestPath":
    case "findAllPaths":
      stmt = compileRelationQueryStatement(modelName, opts);
      break;
    default:
      throw new Error(`Unknown operation type: ${op}`);
  }

  return makeIR(stmt);
}
