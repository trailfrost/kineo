import type { Compiler } from "@/adapter";
import * as IR from "@/ir";

/**
 * Parameters.
 */
type Params = Record<string, any>;

/**
 * Compiles an IR to Cypher.
 * @param ir The IR to compile.
 * @returns A compilation result.
 */
export const compile: Compiler = (ir) => {
  const ctx = createCompileContext();
  const chunks: string[] = [];

  for (const stmt of ir.statements) {
    switch (stmt.type) {
      case IR.StatementType.Find:
        chunks.push(compileFindStatement(ctx, stmt as IR.FindStatement));
        break;
      case IR.StatementType.Count:
        chunks.push(compileCountStatement(ctx, stmt as IR.CountStatement));
        break;
      case IR.StatementType.Create:
        chunks.push(compileCreateStatement(ctx, stmt as IR.CreateStatement));
        break;
      case IR.StatementType.Upsert:
        chunks.push(compileUpsertStatement(ctx, stmt as IR.UpdateStatement));
        break;
      case IR.StatementType.Delete:
        chunks.push(compileDeleteStatement(ctx, stmt as IR.DeleteStatement));
        break;
      case IR.StatementType.ConnectQuery:
        chunks.push(
          compileConnectStatement(ctx, stmt as IR.ConnectQueryStatement),
        );
        break;
      case IR.StatementType.RelationQuery:
        chunks.push(
          compileRelationStatement(ctx, stmt as IR.RelationQueryStatement),
        );
        break;
      default:
        throw new Error(`Unsupported statement type: ${stmt.type}`);
    }
  }

  return { command: chunks.join("\n\n"), params: ctx.params };
};

/* -------------------------------------------------------------------------- */
/*                               Compiler Context                             */
/* -------------------------------------------------------------------------- */

/**
 * Shared state between compile functions.
 */
interface CompileContext {
  params: Params;
  nextParamName(base?: string): string;
}

/**
 * Creates a shared compiler context.
 * @returns A new compiler context.
 */
function createCompileContext(): CompileContext {
  let idx = 0;
  const params: Params = {};
  const nextParamName = (base = "p") => `${base}_${++idx}`;
  return { params, nextParamName };
}

/* -------------------------------------------------------------------------- */
/*                              Helper Utilities                              */
/* -------------------------------------------------------------------------- */

/**
 * Compiles properties to Cypher.
 * @param ctx The compiler context.
 * @param prefix The prefix of the property.
 * @param props The properties to convert.
 * @returns The compiled properties.
 */
function propsToCypher(
  ctx: CompileContext,
  prefix: string,
  props: Record<string, any>,
): string {
  const entries: string[] = [];
  for (const [k, v] of Object.entries(props || {})) {
    const pname = ctx.nextParamName(`${prefix}_${k}`.replace(/\W/g, ""));
    ctx.params[pname] = v;
    entries.push(`${k}: $${pname}`);
  }
  return entries.length ? `{ ${entries.join(", ")} }` : "{}";
}

/**
 * Convert recursive where object into a valid Cypher boolean expression.
 * @param ctx The compiler context.
 * @param alias The result alias.
 * @param where The where object.
 */
function whereToCypher(
  ctx: CompileContext,
  alias: string,
  where?: Record<string, any>,
): string {
  if (!where || Object.keys(where).length === 0) return "1=1";

  const opMap: Record<string, (f: string, val: string) => string> = {
    gt: (f, v) => `${f} > $${v}`,
    gte: (f, v) => `${f} >= $${v}`,
    lt: (f, v) => `${f} < $${v}`,
    lte: (f, v) => `${f} <= $${v}`,
    contains: (f, v) => `${f} CONTAINS $${v}`,
    startsWith: (f, v) => `${f} STARTS WITH $${v}`,
    endsWith: (f, v) => `${f} ENDS WITH $${v}`,
    in: (f, v) => `${f} IN $${v}`,
    not: (f, v) => `${f} <> $${v}`,
  };

  const parts: string[] = [];

  for (const [key, value] of Object.entries(where)) {
    if (key === "AND" || key === "OR" || key === "NOT") continue;

    const field = `${alias}.${key}`;

    if (value && typeof value === "object" && !Array.isArray(value)) {
      for (const [op, val] of Object.entries(value)) {
        const pname = ctx.nextParamName(key);
        ctx.params[pname] = val;
        const opHandler = opMap[op];
        parts.push(
          opHandler ? opHandler(field, pname) : `${field} = $${pname}`,
        );
      }
    } else if (Array.isArray(value)) {
      const pname = ctx.nextParamName(key);
      ctx.params[pname] = value;
      parts.push(`${field} IN $${pname}`);
    } else {
      const pname = ctx.nextParamName(key);
      ctx.params[pname] = value;
      parts.push(`${field} = $${pname}`);
    }
  }

  // Now handle logical operators
  const logicParts: string[] = [];

  if ("AND" in where) {
    const subs = (where.AND as any[]).map((w) => whereToCypher(ctx, alias, w));
    logicParts.push(`(${subs.join(" AND ")})`);
  }
  if ("OR" in where) {
    const subs = (where.OR as any[]).map((w) => whereToCypher(ctx, alias, w));
    logicParts.push(`(${subs.join(" OR ")})`);
  }
  if ("NOT" in where) {
    logicParts.push(`(NOT ${whereToCypher(ctx, alias, where.NOT as any)})`);
  }

  if (parts.length && logicParts.length) {
    return `(${parts.join(" AND ")} AND ${logicParts.join(" AND ")})`;
  }

  return parts.length
    ? `(${parts.join(" AND ")})`
    : logicParts.length
      ? logicParts.join(" AND ")
      : "1=1";
}

/**
 * Projects `select`/`include` objects into Cypher.
 * @param alias The return alias.
 * @param select The select projection.
 * @param include The include projection.
 * @returns A Cypher string.
 */
function projection(
  alias: string,
  select?: Record<string, any>,
  include?: Record<string, any>,
): string {
  const fields: string[] = [];

  if (select && Object.keys(select).length) {
    for (const key of Object.keys(select)) {
      fields.push(`${alias}.${key} AS ${key}`);
    }
  } else {
    fields.push(`properties(${alias}) AS ${alias}`);
  }

  if (include) {
    const collectFields = collectIncludeProjections(alias, include);
    fields.push(...collectFields);
  }

  return fields.join(", ");
}

/**
 * Collects projections from an include object.
 * @param parentAlias The return alias.
 * @param include The include object.
 * @param acc Accumulator.
 * @returns Compiled Cypher chunks.
 */
function collectIncludeProjections(
  parentAlias: string,
  include: Record<string, any>,
  acc: string[] = [],
): string[] {
  for (const [relName, relOpts] of Object.entries(include)) {
    const relAlias = `${parentAlias}_${relName}`;
    const label = (relOpts as any)?.as ?? relName;
    acc.push(`collect(properties(${relAlias})) AS ${label}`);
    if ((relOpts as any)?.include) {
      collectIncludeProjections(relAlias, (relOpts as any).include, acc);
    }
  }
  return acc;
}

/* -------------------------------------------------------------------------- */
/*                            Statement Compilers                             */
/* -------------------------------------------------------------------------- */

/**
 * Compiles a Find statement.
 * @param ctx The compiler context.
 * @param s The statement.
 * @returns A Cypher query.
 */
function compileFindStatement(
  ctx: CompileContext,
  s: IR.FindStatement,
): string {
  const alias = s.alias ?? "n";
  const match = `MATCH (${alias}:${s.model})`;
  const where = `WHERE ${whereToCypher(ctx, alias, s.where)}`;

  const includeMatches = compileIncludesRecursive(ctx, alias, s.include);

  const orderBy =
    s.orderBy && s.orderBy.length
      ? `ORDER BY ${s.orderBy
          .map((o) => {
            const [[field, [, dir]]] = Object.entries(o);
            return `${alias}.${field} ${dir.toUpperCase()}`;
          })
          .join(", ")}`
      : "";

  const skip = typeof s.skip === "number" ? `SKIP ${s.skip}` : "";
  const take = typeof s.take === "number" ? `LIMIT ${s.take}` : "";

  const ret = `RETURN ${projection(alias, s.select, s.include)}`;

  return [match, where, ...includeMatches, ret, orderBy, skip, take]
    .filter(Boolean)
    .join("\n");
}

/**
 * Compiles includes recursively.
 * @param ctx Compiler context.
 * @param parentAlias The return alias.
 * @param include The include object.
 * @param depth The current include depth.
 * @returns Cypher queries.
 */
function compileIncludesRecursive(
  ctx: CompileContext,
  parentAlias: string,
  include?: Record<string, any>,
  depth = 0,
): string[] {
  if (!include) return [];

  const lines: string[] = [];

  for (const [relName, relOpts] of Object.entries(include)) {
    const relAlias = `${parentAlias}_${relName}`;
    const whereClause =
      relOpts && typeof relOpts === "object" && "where" in relOpts
        ? `WHERE ${whereToCypher(ctx, relAlias, (relOpts as any).where)}`
        : "";

    // MATCH the relationship
    lines.push(
      [
        `OPTIONAL MATCH (${parentAlias})-[:${relName.toUpperCase()}]->(${relAlias}:${relName})`,
        whereClause,
      ]
        .filter(Boolean)
        .join("\n"),
    );

    // Recursively compile nested includes
    if (relOpts && typeof relOpts === "object" && "include" in relOpts) {
      const nested = compileIncludesRecursive(
        ctx,
        relAlias,
        (relOpts as any).include,
        depth + 1,
      );
      lines.push(...nested);
    }
  }

  return lines;
}

/**
 * Compiles a Count statement.
 * @param ctx The context.
 * @param s The statement.
 * @returns A Cypher query.
 */
function compileCountStatement(
  ctx: CompileContext,
  s: IR.CountStatement,
): string {
  const alias = s.alias ?? "n";
  return [
    `MATCH (${alias}:${s.model})`,
    `WHERE ${whereToCypher(ctx, alias, s.where)}`,
    `RETURN count(${alias}) AS count`,
  ].join("\n");
}

/**
 * Compiles a Create statement.
 * @param ctx The context.
 * @param s The statement.
 * @returns A Cypher query.
 */
function compileCreateStatement(
  ctx: CompileContext,
  s: IR.CreateStatement,
): string {
  const alias = s.alias ?? "n";
  const props = propsToCypher(ctx, "create", s.data || {});
  const create = `CREATE (${alias}:${s.model} ${props})`;
  return [create, `RETURN ${projection(alias, s.select, s.include)}`].join(
    "\n",
  );
}

/**
 * Compiles an Upsert statement.
 * @param ctx The context.
 * @param s The statement.
 * @returns A Cypher query.
 */
function compileUpsertStatement(
  ctx: CompileContext,
  s: IR.UpdateStatement,
): string {
  const alias = s.alias ?? "n";

  // If there are no where keys, fallback to simple create
  if (!s.where || Object.keys(s.where).length === 0) {
    const props = propsToCypher(
      ctx,
      "upsert_create",
      (s.data as any).create || {},
    );
    const create = `CREATE (${alias}:${s.model} ${props})`;
    return [create, `RETURN properties(${alias}) AS ${alias}`].join("\n");
  }

  // Otherwise, use MERGE with where keys
  const mergeProps = propsToCypher(ctx, "merge", s.where);
  const merge = `MERGE (${alias}:${s.model} ${mergeProps})`;

  const createData = (s.data as any).create || {};
  const updateData = (s.data as any).update || {};

  const onCreate = Object.entries(createData).length
    ? `ON CREATE SET ${Object.entries(createData)
        .map(([k, v]) => {
          const p = ctx.nextParamName(`oncreate_${k}`);
          ctx.params[p] = v;
          return `${alias}.${k} = $${p}`;
        })
        .join(", ")}`
    : "";

  const onMatch = Object.entries(updateData).length
    ? `ON MATCH SET ${Object.entries(updateData)
        .map(([k, v]) => {
          const p = ctx.nextParamName(`onmatch_${k}`);
          ctx.params[p] = v;
          return `${alias}.${k} = $${p}`;
        })
        .join(", ")}`
    : "";

  return [merge, onCreate, onMatch, `RETURN properties(${alias}) AS ${alias}`]
    .filter(Boolean)
    .join("\n");
}

/**
 * Compiles a Delete statement.
 * @param ctx The context.
 * @param s The statement.
 * @returns A Cypher query.
 */
function compileDeleteStatement(
  ctx: CompileContext,
  s: IR.DeleteStatement,
): string {
  const alias = s.alias ?? "n";
  return [
    `MATCH (${alias}:${s.model})`,
    `WHERE ${whereToCypher(ctx, alias, s.where)}`,
    `DELETE ${alias}`,
  ].join("\n");
}

/**
 * Compiles a Connect query statement.
 * @param ctx The context.
 * @param s The statement.
 * @returns A Cypher query.
 */
function compileConnectStatement(
  ctx: CompileContext,
  s: IR.ConnectQueryStatement,
): string {
  const from = "a";
  const to = "b";
  const rel = s.relation.toUpperCase();
  const relProps = propsToCypher(ctx, "rel", s.properties || {});

  const direction =
    s.direction === "IN"
      ? `<-[r:${rel} ${relProps}]-`
      : s.direction === "BOTH"
        ? `-[r:${rel} ${relProps}]-`
        : `-[r:${rel} ${relProps}]->`;

  return [
    `MATCH (${from}:${s.model})`,
    `WHERE ${whereToCypher(ctx, from, s.from)}`,
    `MATCH (${to}:${s.model})`,
    `WHERE ${whereToCypher(ctx, to, s.to)}`,
    `MERGE (${from})${direction}(${to})`,
    `RETURN properties(r) AS relation`,
  ].join("\n");
}

/**
 * Compiles a direction.
 * @param min The minimum value.
 * @param max The maximum value.
 * @param direction The direction.
 */
function directionalRel(
  min: number,
  max: number,
  direction?: "IN" | "OUT" | "BOTH",
) {
  switch (direction) {
    case "IN":
      return `<-[:*${min}..${max}]->`;
    case "OUT":
      return `-[:*${min}..${max}]->`;
    case "BOTH":
    default:
      return `-[:*${min}..${max}]-`;
  }
}
/**
 * Compiles a Relation query statement.
 * @param ctx The context.
 * @param s The statement.
 * @returns A Cypher query.
 */
function compileRelationStatement(
  ctx: CompileContext,
  s: IR.RelationQueryStatement,
): string {
  const from = "a";
  const to = "b";
  const min = s.minDepth ?? 1;
  const max = s.maxDepth ?? s.limit ?? 5;
  const limit = s.limit ? `LIMIT ${s.limit}` : "";

  const relPattern = directionalRel(min, max, s.direction as any);
  const path = `p = (${from})${relPattern}(${to})`;

  return [
    `MATCH (${from}:${s.model})`,
    `WHERE ${whereToCypher(ctx, from, s.from)}`,
    `MATCH (${to}:${s.model})`,
    `WHERE ${whereToCypher(ctx, to, s.to)}`,
    `MATCH ${path}`,
    `RETURN p`,
    limit,
  ]
    .filter(Boolean)
    .join("\n");
}
