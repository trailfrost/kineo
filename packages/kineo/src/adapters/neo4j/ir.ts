import type { IRBase } from "../../ir";

/**
 * A query to get all node labels.
 */
export interface IRGetNodeLabels extends IRBase {
  type: "GET_NODE_LABELS";
}

/**
 * A query to get relationship types.
 */
export interface IRGetRelationshipTypes extends IRBase {
  type: "GET_RELATIONSHIP_TYPES";
}

/**
 * A query to get properties from a node.
 */
export interface IRGetNodeProperties extends IRBase {
  type: "GET_NODE_PROPERTIES";
}

/**
 * A query to get properties from a relationship.
 */
export interface IRGetRelationshipProperties extends IRBase {
  type: "GET_RELATIONSHIP_PROPERTIES";
  relationType: string;
}

/**
 * Parses a "get node labels" query.
 * @param label The label of the statement.
 * @param alias Name of return value.
 * @returns A `GET_NODE_LABELS` statement.
 */
export function parseGetNodeLabels(
  label: string,
  alias: string,
): IRGetNodeLabels {
  return {
    type: "GET_NODE_LABELS",
    label,
    alias,
  };
}

/**
 * Parse a "get relationship types" query.
 * @param label The label of the statement.
 * @param alias Name of return value.
 * @returns A `GET_RELATIONSHIP_TYPES` query.
 */
export function parseGetRelationshipTypes(
  label: string,
  alias: string,
): IRGetRelationshipTypes {
  return {
    type: "GET_RELATIONSHIP_TYPES",
    label,
    alias,
  };
}

/**
 * Parses a "get node properties" statement.
 * @param label The label of the statement.
 * @param alias Name of return value.
 * @returns A `GET_NODE_PROPERTIES` statement.
 */
export function parseGetNodeProperties(
  label: string,
  alias: string,
): IRGetNodeProperties {
  return {
    type: "GET_NODE_PROPERTIES",
    label,
    alias,
  };
}

export function parseGetRelationshipProperties(
  label: string,
  alias: string,
  relationType: string,
): IRGetRelationshipProperties {
  return {
    type: "GET_RELATIONSHIP_PROPERTIES",
    label,
    alias,
    relationType,
  };
}
