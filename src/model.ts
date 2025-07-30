import type * as Neo4j from "neo4j-driver";
import type { Schema, Node, InferNode, RelationshipDef } from "./schema";

// TODO actually build and fire commands

// Utility to extract relationship definitions
type RelationshipKeys<N extends Node> = {
  // eslint-disable-next-line
  [K in keyof N]: N[K] extends RelationshipDef<any> ? K : never;
}[keyof N];

// Extract the "To" key from a relationship field
type GetRelationshipTargetLabel<N extends Node, R extends keyof N> =
  N[R] extends RelationshipDef<infer To> ? To : never;

// Get the full InferNode for the target node
type GetTargetNodeType<
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

type WhereField<T> = {
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

type WhereNode<T> = {
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

export type CypherParams = Record<string, unknown>;

export type CypherResult<T = unknown> = {
  records: T[];
  raw: unknown;
};

export default class Model<S extends Schema, N extends Node> {
  node: N;
  session: Neo4j.Session;

  constructor(node: N, session: Neo4j.Session) {
    this.node = node;
    this.session = session;
  }

  async matchOne(opts?: QueryOpts<S, N>) {
    throw new Error("not implemented");
  }

  async matchMany(opts?: QueryOpts<S, N>) {
    throw new Error("not implemented");
  }

  async exists(opts: Where<S, N>) {
    throw new Error("not implemented");
  }

  async createOne(opts: CreateOpts<S, N>) {
    throw new Error("not implemented");
  }

  async createMany(opts: CreateOpts<S, N>) {
    throw new Error("not implemented");
  }

  async mergeOne(opts: MergeOpts<S, N>) {
    throw new Error("not implemented");
  }

  async mergeMany(opts: MergeOpts<S, N>) {
    throw new Error("not implemented");
  }

  async deleteOne(opts: DeleteOpts<S, N>) {
    throw new Error("not implemented");
  }

  async deleteMany(opts?: DeleteOpts<S, N>) {
    throw new Error("not implemented");
  }

  async connect(opts: ConnectOpts<S, N>) {
    throw new Error("not implemented");
  }

  async disconnect(opts: ConnectOpts<S, N>) {
    throw new Error("not implemented");
  }

  async isConnected(opts: IsConnectedOpts<S, N>) {
    throw new Error("not implemented");
  }

  async upsertRelation(opts: UpsertConnectOpts<S, N>) {
    throw new Error("not implemented");
  }

  async deleteRelation(opts: DeleteConnectionOpts<S, N>) {
    throw new Error("not implemented");
  }

  async getRelations(opts: GetRelationOpts<S, N>) {
    throw new Error("not implemented");
  }

  async getNodeLabels() {
    throw new Error("not implemented");
  }

  async getRelationshipTypes() {
    throw new Error("not implemented");
  }

  async getNodeProperties(label: string) {
    throw new Error("not implemented");
  }

  async getRelationshipProperties(type: string) {
    throw new Error("not implemented");
  }

  async count(opts?: Where<S, N>) {
    throw new Error("not implemented");
  }
}
