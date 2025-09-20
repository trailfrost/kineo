import type { Compiler } from "../adapter";
import type {
  IRDelete,
  IRCreate,
  IRMatch,
  IRMerge,
  IRStatement,
  IRWhereNode,
} from "../ir";
import { IRConnect, IRRelationQuery } from "../graph/ir";
import {
  IRGetNodeLabels,
  IRGetNodeProperties,
  IRGetRelationshipTypes,
  IRGetRelationshipProperties,
} from "../adapters/neo4j/ir";

/**
 * Compiles an intermediate representation to Cypher.
 * @param ir Intermediate representation to compile.
 * @returns The compiled Cypher command.
 */
const compile: Compiler = (ir) => {
  const parts: string[] = [];
  const params: Record<string, unknown> = {};

  ir.statements.forEach((stmt, i) => {
    const result = compileStatement(stmt, i);
    parts.push(result.command);
    Object.assign(params, result.params);
  });

  return {
    command: parts.join(" "),
    params,
  };
};

export default compile;

/**
 * Compiles a statement into Cypher.
 * @param stmt The statement to compile.
 * @param index The index of the statement.
 * @returns Compiled Cypher.
 */
function compileStatement(stmt: IRStatement, index: number) {
  switch (stmt.type) {
    case "MATCH":
      return compileMatch(stmt, index);
    case "CREATE":
      return compileCreate(stmt, index);
    case "MERGE":
      return compileMerge(stmt, index);
    case "DELETE":
      return compileDelete(stmt, index);
    case "CONNECT":
    case "DISCONNECT":
      return compileConnect(stmt, index);
    case "RELATION_QUERY":
      return compileRelationQuery(stmt, index);
    case "GET_NODE_LABELS":
      return compileGetNodeLabels(stmt);
    case "GET_NODE_PROPERTIES":
      return compileGetNodeProperties(stmt);
    case "GET_RELATIONSHIP_PROPERTIES":
      return compileGetRelationshipProperties(stmt);
    case "GET_RELATIONSHIP_TYPES":
      return compileGetRelationshipTypes(stmt);
    default:
      throw new Error(`Unsupported IR statement: ${stmt}`);
  }
}

/**
 * Compiles a `MATCH` statement into Cypher.
 * @param stmt The `MATCH` statement.
 * @param idx The index of the statement.
 * @returns Compiled Cypher.
 */
function compileMatch(stmt: IRMatch, idx: number) {
  const alias = stmt.alias;
  const label = stmt.label;
  const whereRes = compileWhere(stmt.where, alias, idx);

  let query = `MATCH (${alias}:${label})`;
  if (whereRes.clauses.length)
    query += ` WHERE ${whereRes.clauses.join(" AND ")}`;

  if (stmt.orderBy) {
    const order = Object.entries(stmt.orderBy)
      .map(([field, dir]) => `${alias}.${field} ${dir.toUpperCase()}`)
      .join(", ");
    query += ` ORDER BY ${order}`;
  }

  if (stmt.skip !== undefined) query += ` SKIP $skip${idx}`;
  if (stmt.limit !== undefined) query += ` LIMIT $limit${idx}`;
  query += ` RETURN ${stmt.select?.join(", ") || alias}`;

  return {
    command: query,
    params: {
      ...whereRes.params,
      ...(stmt.skip !== undefined ? { [`skip${idx}`]: stmt.skip } : {}),
      ...(stmt.limit !== undefined ? { [`limit${idx}`]: stmt.limit } : {}),
    },
  };
}

/**
 * Compiles a `CREATE` statement into Cypher.
 * @param stmt The `CREATE` statement.
 * @param idx The index of the statement.
 * @returns Compiled Cypher.
 */
function compileCreate(stmt: IRCreate, idx: number) {
  const alias = stmt.alias;
  const label = stmt.label;
  const propsKey = `props${idx}`;

  const command = `
    CREATE (${alias}:${label} $${propsKey})
    RETURN ${alias}
  `;
  return {
    command,
    params: { [propsKey]: stmt.data },
  };
}

/**
 * Compiles a `MERGE` statement into Cypher.
 * @param stmt The `MERGE` statement.
 * @param idx The index of the statement.
 * @returns Compiled Cypher.
 */
function compileMerge(stmt: IRMerge, idx: number) {
  const alias = stmt.alias;
  const label = stmt.label;
  const matchObj = compileMatchObject(stmt.where, alias, idx);

  const queryLines = [
    `MERGE (${alias}:${label} { ${Object.keys(stmt.where)
      .map((k) => `${k}: $${alias}_${k}_${idx}_0`)
      .join(", ")} })`,
  ];

  const params: Record<string, unknown> = { ...matchObj.params };

  if (stmt.update && Object.keys(stmt.update).length > 0) {
    const setLines = Object.entries(stmt.update).map(([key, value], i) => {
      const paramKey = `${alias}_update_${key}_${idx}_${i}`;
      params[paramKey] = value;
      return `${alias}.${key} = $${paramKey}`;
    });
    queryLines.push(`ON MATCH SET ${setLines.join(", ")}`);
  }

  if (stmt.create && Object.keys(stmt.create).length > 0) {
    const setLines = Object.entries(stmt.create).map(([key, value], i) => {
      const paramKey = `${alias}_create_${key}_${idx}_${i}`;
      params[paramKey] = value;
      return `${alias}.${key} = $${paramKey}`;
    });
    queryLines.push(`ON CREATE SET ${setLines.join(", ")}`);
  }

  queryLines.push(`RETURN ${alias}`);

  return {
    command: queryLines.join(" "),
    params,
  };
}

/**
 * Compiles a `DELETE` statement into Cypher.
 * @param stmt The statement.
 * @param idx The index of the statement.
 * @returns Compiled Cypher.
 */
function compileDelete(stmt: IRDelete, idx: number) {
  const alias = stmt.alias;
  const label = stmt.label;

  const matchClause = `MATCH (${alias}:${label})`;
  const whereClause = compileMatchObject(stmt.where ?? {}, alias, idx);

  const query = `
    ${matchClause}${whereClause.clause ? ` WHERE ${whereClause.clause}` : ""}
    DELETE ${alias}
    RETURN ${alias}
  `;
  return {
    command: query,
    params: whereClause.params,
  };
}

/**
 * Compiles a connect statement.
 * @param stmt The statement.
 * @param idx The index of the statement.
 * @returns Compiled Cypher.
 */
function compileConnect(stmt: IRConnect, idx: number) {
  const from = stmt.from;
  const to = stmt.to;
  const rel = stmt.relation;

  const fromMatch = compileMatchObject(from.match, from.alias, idx);
  const toMatch = compileMatchObject(to.match, to.alias, idx);

  const clauses = [
    `MATCH (${from.alias}:${from.label})`,
    `WHERE ${fromMatch.clause}`,
    `MATCH (${to.alias}:${to.label})`,
    `WHERE ${toMatch.clause}`,
  ];

  if (stmt.type === "DISCONNECT") {
    clauses.push(`MATCH (${from.alias})-[r:${rel}]->(${to.alias})`);
    clauses.push(`DELETE r`);
  } else {
    clauses.push(`CREATE (${from.alias})-[:${rel}]->(${to.alias})`);
  }

  clauses.push(`RETURN ${from.alias}`);

  return {
    command: clauses.join(" "),
    params: {
      ...fromMatch.params,
      ...toMatch.params,
    },
  };
}

/**
 * Compiles a relation query.
 * @param stmt The statement.
 * @param idx The index of the statement.
 * @returns Compiled Cypher.
 */
function compileRelationQuery(stmt: IRRelationQuery, idx: number) {
  const from = stmt.from;
  const rel = stmt.relation;
  const alias = stmt.alias;

  const matchFrom = compileMatchObject(from.match, from.alias, idx);
  const relWhere = compileMatchObject(stmt.where ?? {}, alias, idx);

  const commandLines: string[] = [];

  // MATCH the source node
  commandLines.push(`MATCH (${from.alias}:${from.label})`);
  if (matchFrom.clause) {
    commandLines.push(`WHERE ${matchFrom.clause}`);
  }

  // MATCH the relationship
  commandLines.push(
    `MATCH (${from.alias})-[:${rel}]->(${alias}:${stmt.label})`,
  );
  if (relWhere.clause) {
    commandLines.push(`WHERE ${relWhere.clause}`);
  }

  // RETURN the target node(s)
  commandLines.push(`RETURN ${alias}`);

  return {
    command: commandLines.join(" "),
    params: {
      ...matchFrom.params,
      ...relWhere.params,
    },
  };
}

/**
 * Compiles a `WHERE` clause.
 * @param stmt The statement.
 * @param idx The index of the statement.
 * @returns Compiled Cypher.
 */
function compileWhere(
  node: IRWhereNode | undefined,
  alias: string,
  idx: number,
): {
  clauses: string[];
  params: Record<string, unknown>;
} {
  if (!node) return { clauses: [], params: {} };

  const clauses: string[] = [];
  const params: Record<string, unknown> = {};
  let counter = 0;

  const walk = (n: IRWhereNode): string => {
    const inner: string[] = [];

    if (n.conditions) {
      for (const cond of n.conditions) {
        const paramKey = `${alias}_${cond.field}_${idx}_${counter++}`;
        let clause = `${alias}.${cond.field}`;
        switch (cond.operator) {
          case "EQUALS":
            clause += ` = $${paramKey}`;
            break;
          case "IN":
            clause += ` IN $${paramKey}`;
            break;
          case "NOT_IN":
            clause += ` NOT IN $${paramKey}`;
            break;
          case "LT":
            clause += ` < $${paramKey}`;
            break;
          case "LTE":
            clause += ` <= $${paramKey}`;
            break;
          case "GT":
            clause += ` > $${paramKey}`;
            break;
          case "GTE":
            clause += ` >= $${paramKey}`;
            break;
          case "CONTAINS":
            clause += ` CONTAINS $${paramKey}`;
            break;
          case "STARTS_WITH":
            clause += ` STARTS WITH $${paramKey}`;
            break;
          case "ENDS_WITH":
            clause += ` ENDS WITH $${paramKey}`;
            break;
          case "NOT":
            clause = `NOT (${clause} = $${paramKey})`;
            break;
        }
        inner.push(clause);
        params[paramKey] = cond.value;
      }
    }

    if (n.AND) inner.push(...n.AND.map(walk).map((s) => `(${s})`));
    if (n.OR)
      inner.push(
        n.OR.map(walk)
          .map((s) => `(${s})`)
          .join(" OR "),
      );
    if (n.NOT) inner.push(`NOT (${walk(n.NOT)})`);

    return inner.join(" AND ");
  };

  const finalClause = walk(node);
  if (finalClause) clauses.push(finalClause);

  return { clauses, params };
}

/**
 * Compiles a match object.
 * @param stmt The statement.
 * @param idx The index of the statement.
 * @returns Compiled Cypher.
 */
function compileMatchObject(
  where: Record<string, unknown>,
  alias: string,
  idx: number,
) {
  const clauses: string[] = [];
  const params: Record<string, unknown> = {};
  let counter = 0;

  for (const [key, value] of Object.entries(where)) {
    const paramKey = `${alias}_${key}_${idx}_${counter++}`;
    clauses.push(`${alias}.${key} = $${paramKey}`);
    params[paramKey] = value;
  }

  return {
    clause: clauses.join(" AND "),
    params,
  };
}

/**
 * Compiles a `GET_NODE_LABELS` statement.
 * @param stmt The statement to compile.
 * @returns A Cypher query.
 */
function compileGetNodeLabels(stmt: IRGetNodeLabels) {
  return {
    command: `CALL db.labels() YIELD label as ${stmt.alias}`,
    params: {},
  };
}

/**
 * Compiles a `GET_NODE_PROPERTIES` statement.
 * @param stmt The statement to compile.
 * @returns A Cypher query.
 */
function compileGetNodeProperties(stmt: IRGetNodeProperties) {
  return {
    command: `CALL db.schema.nodeTypeProperties() YIELD nodeLabels, propertyName WHERE $label IN nodeLabels RETURN DISTINCT propertyName AS ${stmt.alias}`,
    params: { label: stmt.label },
  };
}

function compileGetRelationshipTypes(stmt: IRGetRelationshipTypes) {
  return {
    command: `CALL db.relationshipTypes() YIELD relationshipType as ${stmt.alias}`,
    params: {},
  };
}

function compileGetRelationshipProperties(stmt: IRGetRelationshipProperties) {
  return {
    command: `CALL db.schema.relTypeProperties() YIELD relType, propertyName WHERE relType = $type RETURN DISTINCT propertyName as ${stmt.alias}`,
    params: { type: stmt.relationType },
  };
}
