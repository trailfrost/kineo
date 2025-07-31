import type { Schema, Node } from "./schema";
import type {
  QueryOpts,
  CreateOpts,
  MergeOpts,
  DeleteOpts,
  ConnectOpts,
  GetRelationOpts,
  WhereNode as SchemaWhereNode,
} from "./model";

export type IRQueryType =
  | "MATCH"
  | "CREATE"
  | "MERGE"
  | "DELETE"
  | "CONNECT"
  | "DISCONNECT"
  | "RELATION_QUERY";

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

export interface IRWhereNode {
  AND?: IRWhereNode[];
  OR?: IRWhereNode[];
  NOT?: IRWhereNode;
  conditions?: IRWhereClause[];
}

export interface IRBase {
  type: IRQueryType;
  label: string;
  alias: string;
}

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

export interface IRCreate extends IRBase {
  type: "CREATE";
  data: Record<string, unknown>;
}

export interface IRMerge extends IRBase {
  type: "MERGE";
  where: Record<string, unknown>;
  update?: Record<string, unknown>;
  create?: Record<string, unknown>;
}

export interface IRDelete extends IRBase {
  type: "DELETE";
  where?: Record<string, unknown>;
}

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

export type IRStatement =
  | IRMatch
  | IRCreate
  | IRMerge
  | IRDelete
  | IRConnect
  | IRRelationQuery;

export interface IR {
  statements: IRStatement[];
}

// --------- WHERE CLAUSE PARSING ---------

function parseWhereField(
  field: string,
  fieldValue: Record<string, unknown>
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

function parseWhereNode<T>(node: SchemaWhereNode<T>): IRWhereNode {
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

export function parseMatch<S extends Schema, N extends Node>(
  label: string,
  alias: string,
  opts: QueryOpts<S, N>
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

export function parseCreate<S extends Schema, N extends Node>(
  label: string,
  alias: string,
  opts: CreateOpts<S, N>
): IRCreate {
  return {
    type: "CREATE",
    label,
    alias,
    data: opts.data,
  };
}

export function parseMerge<S extends Schema, N extends Node>(
  label: string,
  alias: string,
  opts: MergeOpts<S, N>
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

export function parseDelete<S extends Schema, N extends Node>(
  label: string,
  alias: string,
  opts: DeleteOpts<S, N>
): IRDelete {
  return {
    type: "DELETE",
    label,
    alias,
    where: opts.where,
  };
}

export function parseConnect<S extends Schema, N extends Node>(
  label: string,
  alias: string,
  opts: ConnectOpts<S, N>
): IRConnect {
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
      label: opts.relation as string,
      alias: "to",
      match: opts.to,
    },
    relation: opts.relation as string,
  };
}

export function parseDisconnect<S extends Schema, N extends Node>(
  label: string,
  alias: string,
  opts: ConnectOpts<S, N>
): IRConnect {
  return {
    ...parseConnect(label, alias, opts),
    type: "DISCONNECT",
  };
}

export function parseRelationQuery<S extends Schema, N extends Node>(
  label: string,
  alias: string,
  opts: GetRelationOpts<S, N>
): IRRelationQuery {
  return {
    type: "RELATION_QUERY",
    label,
    alias,
    from: {
      label,
      alias: "from",
      match: opts.from,
    },
    relation: opts.relation as string,
    where: opts.where ?? undefined,
  };
}
