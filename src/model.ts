import type * as Neo4j from "neo4j-driver";
import type { Schema, Node, InferNode, RelationshipDef } from "./schema";
import {
  parseMatch,
  parseCreate,
  parseMerge,
  parseDelete,
  parseConnect,
  parseDisconnect,
  parseRelationQuery,
  IRStatement,
} from "./ir";
import compile from "./compiler";

// Utility to extract relationship definitions
export type RelationshipKeys<N extends Node> = {
  // eslint-disable-next-line
  [K in keyof N]: N[K] extends RelationshipDef<any> ? K : never;
}[keyof N];

// Extract the "To" key from a relationship field
export type GetRelationshipTargetLabel<N extends Node, R extends keyof N> =
  N[R] extends RelationshipDef<infer To> ? To : never;

// Get the full InferNode for the target node
export type GetTargetNodeType<
  S extends Schema,
  N extends Node,
  R extends RelationshipKeys<N>,
> = S[GetRelationshipTargetLabel<N, R>] extends Node
  ? InferNode<S[GetRelationshipTargetLabel<N, R>], S>
  : never;

export type ConnectOpts<
  S extends Schema,
  N extends Node,
  R extends RelationshipKeys<N> = RelationshipKeys<N>,
  FromNode = InferNode<N, S>,
  ToNode = GetTargetNodeType<S, N, R>,
> = {
  from: Partial<FromNode>;
  to: Partial<ToNode>;
  relation: R;
};

export type IsConnectedOpts<
  S extends Schema,
  N extends Node,
  R extends RelationshipKeys<N> = RelationshipKeys<N>,
> = ConnectOpts<S, N, R>;

export type UpsertConnectOpts<
  S extends Schema,
  N extends Node,
  R extends RelationshipKeys<N> = RelationshipKeys<N>,
> = ConnectOpts<S, N, R> & {
  create?: boolean;
};

export type DeleteConnectionOpts<
  S extends Schema,
  N extends Node,
  R extends RelationshipKeys<N> = RelationshipKeys<N>,
> = ConnectOpts<S, N, R>;

export type GetRelationOpts<
  S extends Schema,
  N extends Node,
  R extends RelationshipKeys<N> = RelationshipKeys<N>,
  FromNode = InferNode<N, S>,
> = {
  from: Partial<FromNode>;
  relation: R;
  where?: Partial<GetTargetNodeType<S, N, R>>;
};

export type WhereField<T> = {
  equals?: T;
  in?: T[];
  notIn?: T[];
  lt?: T;
  lte?: T;
  gt?: T;
  gte?: T;
  contains?: T extends string ? string : never;
  startsWith?: T extends string ? string : never;
  endsWith?: T extends string ? string : never;
  not?: WhereField<T> | T;
};

export type WhereNode<T> = {
  AND?: WhereNode<T>[];
  OR?: WhereNode<T>[];
  NOT?: WhereNode<T>;
} & {
  [K in keyof T]?: T[K] extends Array<infer U>
    ? WhereNode<U> | WhereField<U>
    : T[K] extends Date | number | string | boolean | null | undefined
      ? WhereField<T[K]>
      : T[K] extends object
        ? WhereNode<T[K]>
        : never;
};

export type Where<S extends Schema, N extends Node> = WhereNode<
  InferNode<N, S>
>;

export interface QueryOpts<
  S extends Schema,
  N extends Node,
  INode = InferNode<N, S>,
> {
  where?: Where<S, N>;
  orderBy?: {
    [K in keyof INode]?: "asc" | "desc";
  };
  limit?: number;
  skip?: number;
  include?: {
    // eslint-disable-next-line
    [K in keyof N]?: N[K] extends RelationshipDef<any> ? boolean : never;
  };
  select?: {
    [K in keyof INode]?: boolean;
  };
}

export type CreateOpts<
  S extends Schema,
  N extends Node,
  INode = InferNode<N, S>,
> = {
  data: INode;
};

export type MergeOpts<
  S extends Schema,
  N extends Node,
  INode = InferNode<N, S>,
> = {
  where: Partial<INode>;
  update?: Partial<INode>;
  create?: INode;
};

export type DeleteOpts<
  S extends Schema,
  N extends Node,
  INode = InferNode<N, S>,
> = {
  where?: Partial<INode>;
};

export default class Model<S extends Schema, N extends Node> {
  readonly label: string;
  readonly node: N;
  readonly session: Neo4j.Session;

  constructor(label: string, node: N, session: Neo4j.Session) {
    this.label = label;
    this.node = node;
    this.session = session;
  }

  private async run(ir: IRStatement): Promise<Neo4j.QueryResult> {
    const { cypher, params } = compile({ statements: [ir] });
    return this.session.run(cypher, params);
  }

  async matchOne(opts?: QueryOpts<S, N>) {
    const ir = parseMatch(this.label, "n", opts ?? {});
    const result = await this.run(ir);
    return result.records[0] ?? null;
  }

  async matchMany(opts?: QueryOpts<S, N>) {
    const ir = parseMatch(this.label, "n", opts ?? {});
    const result = await this.run(ir);
    return result.records;
  }

  async exists(opts: Where<S, N>) {
    const ir = parseMatch(this.label, "n", { where: opts, limit: 1 });
    const result = await this.run(ir);
    return result.records.length > 0;
  }

  async createOne(opts: CreateOpts<S, N>) {
    const ir = parseCreate(this.label, "n", opts);
    const result = await this.run(ir);
    return result.records[0] ?? null;
  }

  async createMany(opts: CreateOpts<S, N>) {
    const ir = parseCreate(this.label, "n", opts);
    const result = await this.run(ir);
    return result.records;
  }

  async mergeOne(opts: MergeOpts<S, N>) {
    const ir = parseMerge(this.label, "n", opts);
    const result = await this.run(ir);
    return result.records[0] ?? null;
  }

  async mergeMany(opts: MergeOpts<S, N>) {
    const ir = parseMerge(this.label, "n", opts);
    const result = await this.run(ir);
    return result.records;
  }

  async deleteOne(opts: DeleteOpts<S, N>) {
    const ir = parseDelete(this.label, "n", opts);
    const result = await this.run(ir);
    return result.records[0] ?? null;
  }

  async deleteMany(opts?: DeleteOpts<S, N>) {
    const ir = parseDelete(this.label, "n", opts ?? {});
    const result = await this.run(ir);
    return result.records;
  }

  async connect(opts: ConnectOpts<S, N>) {
    const ir = parseConnect(this.label, "n", opts);
    const result = await this.run(ir);
    return result.records[0] ?? null;
  }

  async disconnect(opts: ConnectOpts<S, N>) {
    const ir = parseDisconnect(this.label, "n", opts);
    const result = await this.run(ir);
    return result.records[0] ?? null;
  }

  async isConnected(opts: IsConnectedOpts<S, N>) {
    const ir = parseConnect(this.label, "n", opts);
    const result = await this.run(ir);
    return result.records.length > 0;
  }

  async upsertRelation(opts: UpsertConnectOpts<S, N>) {
    const ir = parseConnect(this.label, "n", opts); // Could differentiate based on `create?: true`
    const result = await this.run(ir);
    return result.records[0] ?? null;
  }

  async deleteRelation(opts: DeleteConnectionOpts<S, N>) {
    const ir = parseDisconnect(this.label, "n", opts);
    const result = await this.run(ir);
    return result.records[0] ?? null;
  }

  async getRelations(opts: GetRelationOpts<S, N>) {
    const ir = parseRelationQuery(this.label, "n", opts);
    const result = await this.run(ir);
    return result.records;
  }

  async count(opts?: Where<S, N>) {
    const ir = parseMatch(this.label, "n", {
      where: opts,
      select: {}, // just match the node, then count
    });
    const { cypher, params } = compile({ statements: [ir] });
    const countQuery = `CALL { ${cypher} } RETURN count(n) as count`;
    const result = await this.session.run(countQuery, params);
    return result.records[0]?.get("count").toInt?.() ?? 0;
  }

  async getNodeLabels(): Promise<string[]> {
    const result = await this.session.run(`CALL db.labels()`);
    return result.records.map((r) => r.get("label"));
  }

  async getRelationshipTypes(): Promise<string[]> {
    const result = await this.session.run(`CALL db.relationshipTypes()`);
    return result.records.map((r) => r.get("relationshipType"));
  }

  async getNodeProperties(label: string): Promise<string[]> {
    const result = await this.session.run(
      `
    CALL db.schema.nodeTypeProperties()
    YIELD nodeType, propertyName
    WHERE $label IN nodeType
    RETURN DISTINCT propertyName
    `,
      { label }
    );
    return result.records.map((r) => r.get("propertyName"));
  }

  async getRelationshipProperties(type: string): Promise<string[]> {
    const result = await this.session.run(
      `
    CALL db.schema.relTypeProperties()
    YIELD relType, propertyName
    WHERE relType = $type
    RETURN DISTINCT propertyName
    `,
      { type }
    );
    return result.records.map((r) => r.get("propertyName"));
  }
}
