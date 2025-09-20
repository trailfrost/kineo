import type { Compiler } from "../adapter";
import type { IRStatement, IRMatch, IRCreate, IRMerge, IRDelete } from "../ir";

/**
 * A SQL dialect. A mini compiler for specific sections of SQL.
 */
export interface Dialect {
  /**
   * Compiles an identifier.
   * @param name The name of the identifier.
   */
  identifier(name: string): string;
  /**
   * Compiles a string.
   * @param value The string value.
   */
  string(value: string): string;
  /**
   * Compiles an array.
   * @param values The array entries.
   */
  array(values: unknown[]): string;
  /**
   * Compiles a limit/offset.
   * @param limit The limit.
   * @param offset The offset.
   */
  limitOffset(limit?: number, offset?: number): string;
  /**
   * Compiles a boolean.
   * @param value The boolean value.
   */
  boolean(value: boolean): string;
  /**
   * Compiles an upsert command.
   * @param table The table to upsert to.
   * @param args All necessary arguments for upserting.
   */
  upsert(table: string, ...args: unknown[]): string;
  /**
   * Compiles an auto-incrementing integer.
   */
  autoIncrement(): string;
  /**
   * Compiles the way to get the current date.
   */
  now(): string;
  /**
   * Compiles a JSON extract.
   * @param column The column to extract.
   * @param path The path to extract.
   */
  jsonExtract(column: string, path: string): string;
  /**
   * Compiles a type.
   * @param type The type to compile.
   */
  type(type: string): string;
  /**
   * Compiles a return.
   * @param columns The columns to return.
   */
  returning(columns?: string[]): string;
}

/**
 *
 * @param ir The intermediate representation.
 * @param dialect The dialect to compile to.
 * @returns The compiled output.
 */
const compile: Compiler<Dialect> = (ir, dialect) => {
  const params: Record<string, unknown> = {};
  const queries = ir.statements.map((stmt) =>
    compileStatement(stmt, dialect, params),
  );
  return {
    command: queries.join(";\n"),
    params,
  };
};

export default compile;

function compileStatement(
  stmt: IRStatement,
  preset: Dialect,
  params: Record<string, unknown>,
): string {
  switch (stmt.type) {
    case "MATCH":
      return compileMatch(stmt, preset, params);
    case "CREATE":
      return compileCreate(stmt, preset, params);
    case "DELETE":
      return compileDelete(stmt, preset, params);
    case "MERGE":
      return compileMerge(stmt, preset, params);
    default:
      throw new Error(`Unsupported statement type: ${stmt.type}`);
  }
}

// statement compilers
function compileMatch(
  stmt: IRMatch,
  preset: Dialect,
  params: Record<string, unknown>,
): string {
  const cols = stmt.select?.map((c) => preset.identifier(c)).join(", ") || "*";
  let sql = `SELECT ${cols} FROM ${preset.identifier(stmt.label)} ${preset.identifier(
    stmt.alias,
  )}`;
  const paramIndex = { i: 1 };

  if (stmt.where) {
    const whereClause = compileWhere(stmt.where, preset, params, paramIndex);
    if (whereClause) sql += ` WHERE ${whereClause}`;
  }

  if (stmt.orderBy) {
    const order = Object.entries(stmt.orderBy)
      .map(([col, dir]) => `${preset.identifier(col)} ${dir.toUpperCase()}`)
      .join(", ");
    sql += ` ORDER BY ${order}`;
  }

  if (stmt.limit !== undefined || stmt.skip !== undefined) {
    sql += " " + preset.limitOffset(stmt.limit, stmt.skip);
  }

  return sql;
}

function compileCreate(
  stmt: IRCreate,
  preset: Dialect,
  params: Record<string, unknown>,
): string {
  const cols = Object.keys(stmt.data).map((k) => preset.identifier(k));
  const placeholders = Object.keys(stmt.data).map((k, i) => {
    const pname = `p${i + 1}`;
    params[pname] = stmt.data[k];
    return `:${pname}`;
  });
  return `INSERT INTO ${preset.identifier(stmt.label)} (${cols.join(
    ", ",
  )}) VALUES (${placeholders.join(", ")})`;
}

function compileDelete(
  stmt: IRDelete,
  preset: Dialect,
  params: Record<string, unknown>,
): string {
  let sql = `DELETE FROM ${preset.identifier(stmt.label)}`;
  if (stmt.where && Object.keys(stmt.where).length > 0) {
    const conds = Object.entries(stmt.where).map(([k, v], i) => {
      const pname = `p${i + 1}`;
      params[pname] = v;
      return `${preset.identifier(k)} = :${pname}`;
    });
    sql += " WHERE " + conds.join(" AND ");
  }
  return sql;
}

function compileMerge(
  stmt: IRMerge,
  preset: Dialect,
  params: Record<string, unknown>,
): string {
  // Let the preset handle dialect-specific details
  // Pass in table name, match conditions, update data, create data
  return preset.upsert(
    stmt.label,
    stmt.where,
    stmt.update,
    stmt.create,
    params,
  );
}

// helper for WHERE clause compilation
function compileWhere(
  node: IRMatch["where"],
  preset: Dialect,
  params: Record<string, unknown>,
  paramIndex: { i: number },
): string {
  if (!node) return "";
  const parts: string[] = [];

  if (node.conditions) {
    for (const cond of node.conditions) {
      const col = preset.identifier(cond.field);
      const paramName = `p${paramIndex.i++}`;
      params[paramName] = cond.value;

      switch (cond.operator) {
        case "EQUALS":
          parts.push(`${col} = :${paramName}`);
          break;
        case "IN":
          parts.push(`${col} IN (${preset.array(cond.value as unknown[])})`);
          break;
        case "NOT_IN":
          parts.push(
            `${col} NOT IN (${preset.array(cond.value as unknown[])})`,
          );
          break;
        case "LT":
          parts.push(`${col} < :${paramName}`);
          break;
        case "LTE":
          parts.push(`${col} <= :${paramName}`);
          break;
        case "GT":
          parts.push(`${col} > :${paramName}`);
          break;
        case "GTE":
          parts.push(`${col} >= :${paramName}`);
          break;
        case "CONTAINS":
          parts.push(`${col} LIKE '%' || :${paramName} || '%'`);
          break;
        case "STARTS_WITH":
          parts.push(`${col} LIKE :${paramName} || '%'`);
          break;
        case "ENDS_WITH":
          parts.push(`${col} LIKE '%' || :${paramName}`);
          break;
        case "NOT":
          parts.push(`NOT (${col} = :${paramName})`);
          break;
      }
    }
  }

  if (node.AND) {
    const ands = node.AND.map((n) =>
      compileWhere(n, preset, params, paramIndex),
    ).filter(Boolean);
    if (ands.length) parts.push(`(${ands.join(" AND ")})`);
  }
  if (node.OR) {
    const ors = node.OR.map((n) =>
      compileWhere(n, preset, params, paramIndex),
    ).filter(Boolean);
    if (ors.length) parts.push(`(${ors.join(" OR ")})`);
  }
  if (node.NOT) {
    parts.push(`NOT (${compileWhere(node.NOT, preset, params, paramIndex)})`);
  }

  return parts.join(" AND ");
}
