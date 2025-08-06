import type { Schema, Node, RelationshipDef } from "./schema";
import type {
  QueryOpts,
  CreateOpts,
  MergeOpts,
  DeleteOpts,
  ConnectOpts,
  GetRelationOpts,
  WhereNode as SchemaWhereNode,
} from "./model";

/**
 * All possible statements.
 */
export type IRQueryType =
  | "MATCH"
  | "CREATE"
  | "MERGE"
  | "DELETE"
  | "CONNECT"
  | "DISCONNECT"
  | "RELATION_QUERY";

/**
 * A `WHERE` clause.
 */
export interface IRWhereClause {
  field: string;
  operator:
    | "EQUALS"
    | "IN"
    | "NOT_IN"
    | "LT"
    | "LTE"
    | "GT"
    | "GTE"
    | "CONTAINS"
    | "STARTS_WITH"
    | "ENDS_WITH"
    | "NOT";
  value: unknown;
}

/**
 * A `WHERE` node.
 */
export interface IRWhereNode {
  AND?: IRWhereNode[];
  OR?: IRWhereNode[];
  NOT?: IRWhereNode;
  conditions?: IRWhereClause[];
}

/**
 * Base interface for creating IR types.
 */
export interface IRBase {
  type: IRQueryType;
  label: string;
  alias: string;
}

/**
 * Match statement.
 */
export interface IRMatch extends IRBase {
  type: "MATCH";
  where?: IRWhereNode;
  orderBy?: {
    [K: string]: "asc" | "desc";
  };
  limit?: number;
  skip?: number;
  include?: string[];
  select?: string[];
}

/**
 * Create statement.
 */
export interface IRCreate extends IRBase {
  type: "CREATE";
  data: Record<string, unknown>;
}

/**
 * Merge (upsert) statement.
 */
export interface IRMerge extends IRBase {
  type: "MERGE";
  where: Record<string, unknown>;
  update?: Record<string, unknown>;
  create?: Record<string, unknown>;
}

/**
 * Delete statement.
 */
export interface IRDelete extends IRBase {
  type: "DELETE";
  where?: Record<string, unknown>;
}

/**
 * Connect statement.
 */
export interface IRConnect extends IRBase {
  type: "CONNECT" | "DISCONNECT";
  from: {
    label: string;
    alias: string;
    match: Record<string, unknown>;
  };
  to: {
    label: string;
    alias: string;
    match: Record<string, unknown>;
  };
  relation: string;
}

/**
 * Relation query.
 */
export interface IRRelationQuery extends IRBase {
  type: "RELATION_QUERY";
  from: {
    label: string;
    alias: string;
    match: Record<string, unknown>;
  };
  relation: string;
  where?: Record<string, unknown>;
}

/**
 * All types of statements.
 */
export type IRStatement =
  | IRMatch
  | IRCreate
  | IRMerge
  | IRDelete
  | IRConnect
  | IRRelationQuery;

/**
 * Intermediate representation, built for compiling into Cypher.
 */
export interface IR {
  statements: IRStatement[];
}

// --------- WHERE CLAUSE PARSING ---------

/**
 * Parses a field in a `WHERE` statement.
 * @param field The field to parse.
 * @param fieldValue The value of the field.
 * @returns `WHERE` clauses.
 */
export function parseWhereField(
  field: string,
  fieldValue: Record<string, unknown>,
): IRWhereClause[] {
  const clauses: IRWhereClause[] = [];

  for (const op in fieldValue) {
    const val = fieldValue[op];
    switch (op) {
      case "equals":
        clauses.push({ field, operator: "EQUALS", value: val });
        break;
      case "in":
        clauses.push({ field, operator: "IN", value: val });
        break;
      case "notIn":
        clauses.push({ field, operator: "NOT_IN", value: val });
        break;
      case "lt":
        clauses.push({ field, operator: "LT", value: val });
        break;
      case "lte":
        clauses.push({ field, operator: "LTE", value: val });
        break;
      case "gt":
        clauses.push({ field, operator: "GT", value: val });
        break;
      case "gte":
        clauses.push({ field, operator: "GTE", value: val });
        break;
      case "contains":
        clauses.push({ field, operator: "CONTAINS", value: val });
        break;
      case "startsWith":
        clauses.push({ field, operator: "STARTS_WITH", value: val });
        break;
      case "endsWith":
        clauses.push({ field, operator: "ENDS_WITH", value: val });
        break;
      case "not":
        if (typeof val === "object") {
          const nested = parseWhereField(field, val as typeof fieldValue);
          nested.forEach((n) => clauses.push({ ...n, operator: "NOT" }));
        } else {
          clauses.push({ field, operator: "NOT", value: val });
        }
        break;
    }
  }

  return clauses;
}

/**
 * Parses a `WHERE` node.
 * @param node The node to parse.
 * @returns A `WHERE` node.
 */
export function parseWhereNode<T>(node: SchemaWhereNode<T>): IRWhereNode {
  const result: IRWhereNode = {};

  if (node.AND) {
    result.AND = node.AND.map(parseWhereNode);
  }

  if (node.OR) {
    result.OR = node.OR.map(parseWhereNode);
  }

  if (node.NOT) {
    result.NOT = parseWhereNode(node.NOT);
  }

  const conditions: IRWhereClause[] = [];

  for (const key in node) {
    if (["AND", "OR", "NOT"].includes(key)) continue;

    const val = node[key as keyof T];
    if (val == null) continue;

    if (typeof val === "object" && !Array.isArray(val)) {
      conditions.push(...parseWhereField(key, val));
    }
  }

  if (conditions.length) result.conditions = conditions;
  return result;
}

// --------- STATEMENT PARSERS ---------

/**
 * Parses a match statement.
 * @param label The label of the statement.
 * @param alias Name of the return.
 * @param opts Options passed into the model.
 * @returns A `MATCH` statement.
 */
export function parseMatch<S extends Schema, N extends Node>(
  label: string,
  alias: string,
  opts: QueryOpts<S, N>,
): IRMatch {
  return {
    type: "MATCH",
    label,
    alias,
    where: opts.where ? parseWhereNode(opts.where) : undefined,
    orderBy: opts.orderBy as IRMatch["orderBy"],
    limit: opts.limit,
    skip: opts.skip,
    include: opts.include ? Object.keys(opts.include) : undefined,
    select: opts.select ? Object.keys(opts.select) : undefined,
  };
}

/**
 * Parses a `CREATE` statement.
 * @param label The label of the command.
 * @param alias Name of return value.
 * @param opts Options passed into the model.
 * @returns A `CREATE` statement.
 */
export function parseCreate<S extends Schema, N extends Node>(
  label: string,
  alias: string,
  opts: CreateOpts<S, N>,
): IRCreate {
  return {
    type: "CREATE",
    label,
    alias,
    data: opts.data,
  };
}

/**
 * Parses a merge statement.
 * @param label The label of the statement.
 * @param alias Name of return value.
 * @param opts Options passed into the model.
 * @returns A `MERGE` statement.
 */
export function parseMerge<S extends Schema, N extends Node>(
  label: string,
  alias: string,
  opts: MergeOpts<S, N>,
): IRMerge {
  return {
    type: "MERGE",
    label,
    alias,
    where: opts.where,
    update: opts.update,
    create: opts.create,
  };
}

/**
 * Parses a delete statement.
 * @param label The label of the statement.
 * @param alias Name of return value.
 * @param opts Options passed into the module.
 * @returns A `DELETE` statement.
 */
export function parseDelete<S extends Schema, N extends Node>(
  label: string,
  alias: string,
  opts: DeleteOpts<S, N>,
): IRDelete {
  return {
    type: "DELETE",
    label,
    alias,
    where: opts.where,
  };
}

/**
 * Parses a connect statement.
 * @param label The label of the statement.
 * @param alias Name of return value.
 * @param opts Options passed into the model.
 * @param nodeDef Node definition.
 * @returns A connect statement.
 */
export function parseConnect<S extends Schema, N extends Node>(
  label: string,
  alias: string,
  opts: ConnectOpts<S, N>,
  nodeDef: N,
): IRConnect {
  const relDef = nodeDef[opts.relation] as RelationshipDef<string>;
  const toLabel = relDef.refTo;

  return {
    type: "CONNECT",
    label,
    alias,
    from: {
      label,
      alias: "from",
      match: opts.from,
    },
    to: {
      label: toLabel,
      alias: "to",
      match: opts.to,
    },
    relation: opts.relation as string,
  };
}

/**
 * Parses a disconnect statement.
 * @param label The label of the statement.
 * @param alias Name of return value.
 * @param opts Options passed into the model.
 * @param nodeDef Node definition.
 * @returns A disconnect statement.
 */
export function parseDisconnect<S extends Schema, N extends Node>(
  label: string,
  alias: string,
  opts: ConnectOpts<S, N>,
  nodeDef: N,
): IRConnect {
  return {
    ...parseConnect(label, alias, opts, nodeDef),
    type: "DISCONNECT",
  };
}

/**
 * Parses a relationship query.
 * @param schema Object definitions.
 * @param nodeLabel Name of the node that is related.
 * @param opts Options passed into the model.
 * @returns A relationship query.
 */
export function parseRelationQuery<S extends Schema, N extends Node>(
  schema: S,
  nodeLabel: string,
  opts: GetRelationOpts<S, N>,
): IRRelationQuery {
  const nodeDef = schema[nodeLabel] as N;
  const relDef = nodeDef[opts.relation] as RelationshipDef<string>;

  return {
    type: "RELATION_QUERY",
    label: relDef.refTo,
    alias: "n",
    from: {
      label: nodeLabel,
      alias: "from",
      match: opts.from,
    },
    relation: opts.relation as string,
    where: opts.where ?? undefined,
  };
}
