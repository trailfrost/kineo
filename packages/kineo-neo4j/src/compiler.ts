import type {
  IR,
  IRConnect,
  IRDelete,
  IRCreate,
  IRMatch,
  IRMerge,
  IRRelationQuery,
  IRStatement,
  IRWhereNode,
} from "kineo/ir";

/**
 * Compiles an intermediate representation to Cypher.
 * @param ir Intermediate representation to compile.
 * @returns The compiled Cypher command.
 */
export default function compile(ir: IR) {
  const parts: string[] = [];
  const params: Record<string, unknown> = {};

  ir.statements.forEach((stmt, i) => {
    const result = compileStatement(stmt, i);
    parts.push(result.cypher);
    Object.assign(params, result.params);
  });

  return {
    cypher: parts.join("\n"),
    params,
  };
}

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
  query += ` RETURN ${stmt.select?.join(", ") || alias}`;

  if (stmt.orderBy) {
    const order = Object.entries(stmt.orderBy)
      .map(([field, dir]) => `${alias}.${field} ${dir.toUpperCase()}`)
      .join(", ");
    query += ` ORDER BY ${order}`;
  }

  if (stmt.skip !== undefined) query += ` SKIP $skip${idx}`;
  if (stmt.limit !== undefined) query += ` LIMIT $limit${idx}`;

  return {
    cypher: query,
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

  const cypher = `
    CREATE (${alias}:${label} $${propsKey})
    RETURN ${alias}
  `;
  return {
    cypher,
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
    cypher: queryLines.join("\n"),
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
  const whereClause = compileMatchObject(stmt.where || {}, alias, idx);

  const query = `
    ${matchClause}${whereClause.clause ? ` WHERE ${whereClause.clause}` : ""}
    DELETE ${alias}
    RETURN ${alias}
  `;
  return {
    cypher: query,
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
    cypher: clauses.join("\n"),
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
  const relWhere = compileMatchObject(stmt.where || {}, alias, idx);

  const cypherLines: string[] = [];

  // MATCH the source node
  cypherLines.push(`MATCH (${from.alias}:${from.label})`);
  if (matchFrom.clause) {
    cypherLines.push(`WHERE ${matchFrom.clause}`);
  }

  // MATCH the relationship
  cypherLines.push(`MATCH (${from.alias})-[:${rel}]->(${alias}:${stmt.label})`);
  if (relWhere.clause) {
    cypherLines.push(`WHERE ${relWhere.clause}`);
  }

  // RETURN the target node(s)
  cypherLines.push(`RETURN ${alias}`);

  return {
    cypher: cypherLines.join("\n"),
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
  idx: number
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
          .join(" OR ")
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
  idx: number
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
