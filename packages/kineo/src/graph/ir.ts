import { IRBase } from "../ir";
import { RelationshipDef, Schema, Node } from "../schema";
import { ConnectOpts, GetRelationOpts } from "./model";

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
