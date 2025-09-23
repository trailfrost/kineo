import type { Adapter } from "./adapter.js";
import type { Direction, InferModelDef, ModelDef, Schema } from "./schema.js";

// ---------- Generic Utility Types ---------- //

export type PrimitiveFilter<T> = T extends string
  ? {
      equals?: T;
      in?: T[];
      notIn?: T[];
      contains?: T;
      startsWith?: T;
      endsWith?: T;
      mode?: "default" | "insensitive";
      not?: PrimitiveFilter<T> | T;
    }
  : T extends number
    ? {
        equals?: T;
        in?: T[];
        notIn?: T[];
        lt?: T;
        lte?: T;
        gt?: T;
        gte?: T;
        not?: PrimitiveFilter<T> | T;
      }
    : T extends boolean
      ? { equals?: T; not?: T }
      : T extends Date
        ? {
            equals?: T | string;
            before?: T | string;
            after?: T | string;
            between?: [T | string, T | string];
            not?: T | string;
          }
        : never;

/** Allow direct value or filter object */
export type FieldFilter<T> = T | PrimitiveFilter<T>;

/** Extract ID field from a model */
export type IdOf<M> = {
  [K in keyof M]: M[K] extends infer V
    ? K extends string
      ? V extends string | number
        ? K
        : never
      : never
    : never;
}[keyof M];

// ---------- CRUD Options ---------- //

export interface QueryOpts<
  S extends Schema,
  M extends ModelDef,
  MType = InferModelDef<M, S>,
> {
  where?: {
    [K in keyof MType]?: FieldFilter<MType[K]>;
  };
  select?: {
    [K in keyof MType]?: boolean | QueryOpts<S, any>; // nested select
  };
  include?: {
    [K in keyof MType]?: boolean | QueryOpts<S, any>; // nested include
  };
  orderBy?: {
    [K in keyof MType]?: "asc" | "desc";
  }[];
  distinct?: (keyof MType)[];
  skip?: number;
  take?: number;
}

export interface CreateOpts<
  S extends Schema,
  M extends ModelDef,
  MType = InferModelDef<M, S>,
> {
  data: {
    [K in keyof MType]?: MType[K];
  };
  select?: QueryOpts<S, M>["select"];
  include?: QueryOpts<S, M>["include"];
}

export interface UpdateOpts<
  S extends Schema,
  M extends ModelDef,
  MType = InferModelDef<M, S>,
> {
  where: {
    [K in keyof MType]?: FieldFilter<MType[K]>;
  };
  data: Partial<MType>;
  select?: QueryOpts<S, M>["select"];
  include?: QueryOpts<S, M>["include"];
}

export interface DeleteOpts<
  S extends Schema,
  M extends ModelDef,
  MType = InferModelDef<M, S>,
> {
  where: {
    [K in keyof MType]?: FieldFilter<MType[K]>;
  };
  select?: QueryOpts<S, M>["select"];
  include?: QueryOpts<S, M>["include"];
}

export interface UpsertOpts<
  S extends Schema,
  M extends ModelDef,
  MType = InferModelDef<M, S>,
> {
  where: {
    [K in keyof MType]?: FieldFilter<MType[K]>;
  };
  create: CreateOpts<S, M>["data"];
  update: UpdateOpts<S, M>["data"];
  select?: QueryOpts<S, M>["select"];
  include?: QueryOpts<S, M>["include"];
}

// ---------- Graph-specific Options ---------- //

export interface PathOpts<
  S extends Schema,
  M extends ModelDef,
  MType = InferModelDef<M, S>,
> {
  from: {
    where: { [K in keyof MType]?: FieldFilter<MType[K]> };
  };
  to: {
    where: { [K in keyof MType]?: FieldFilter<MType[K]> };
  };
  maxDepth?: number;
  minDepth?: number;
  direction?: Direction;
  limit?: number;
}

export interface ConnectOpts<
  S extends Schema,
  M extends ModelDef,
  MType = InferModelDef<M, S>,
> {
  from: { where: { [K in keyof MType]?: FieldFilter<MType[K]> } };
  to: { where: { [K in keyof MType]?: FieldFilter<MType[K]> } };
  relation: string; // rel label
  direction?: Direction;
  properties?: Record<string, any>;
}

export interface TraverseOpts<
  S extends Schema,
  M extends ModelDef,
  MType = InferModelDef<M, S>,
> {
  start: { where: { [K in keyof MType]?: FieldFilter<MType[K]> } };
  direction?: Direction;
  depth?: number;
  maxDepth?: number;
  relationFilter?: string | string[];
  includeNodes?: boolean;
  includeEdges?: boolean;
}

// ---------- Result Type Helpers ---------- //

/** If select/include specified, infer shape, otherwise return full model */
export type ResultPayload<
  S extends Schema,
  M extends ModelDef,
  O extends QueryOpts<S, M> | undefined,
  MType = InferModelDef<M, S>,
> = O extends { select: any }
  ? SelectedFields<S, M, NonNullable<O["select"]>, MType>
  : O extends { include: any }
    ? IncludedFields<S, M, NonNullable<O["include"]>, MType>
    : MType;

/** Handle select: pick subset */
export type SelectedFields<
  S extends Schema,
  M extends ModelDef,
  Sel extends Record<string, any>,
  MType = InferModelDef<M, S>,
> = {
  [K in keyof Sel & keyof MType]: Sel[K] extends true
    ? MType[K]
    : Sel[K] extends QueryOpts<S, any>
      ? // nested select: dive into schema
        MType[K] // you could refine further if MType[K] is relational
      : never;
};

/** Handle include: add nested relations */
export type IncludedFields<
  S extends Schema,
  M extends ModelDef,
  Inc extends Record<string, any>,
  MType = InferModelDef<M, S>,
> = MType & {
  [K in keyof Inc & keyof MType]: Inc[K] extends true
    ? MType[K]
    : Inc[K] extends QueryOpts<S, any>
      ? MType[K] // refine further if you want deep type inference
      : never;
};

// ---------- Return Types for CRUD ---------- //

export type FindFirstReturn<
  S extends Schema,
  M extends ModelDef,
  O extends QueryOpts<S, M> | undefined,
> = Promise<ResultPayload<S, M, O> | null>;

export type FindManyReturn<
  S extends Schema,
  M extends ModelDef,
  O extends QueryOpts<S, M> | undefined,
> = Promise<ResultPayload<S, M, O>[]>;

export type CountReturn = Promise<number>;

export type CreateReturn<
  S extends Schema,
  M extends ModelDef,
  O extends CreateOpts<S, M>,
> = Promise<ResultPayload<S, M, O>>;

export type UpdateReturn<
  S extends Schema,
  M extends ModelDef,
  O extends UpdateOpts<S, M>,
> = Promise<ResultPayload<S, M, O>>;

export type UpdateManyReturn = Promise<{ count: number }>;

export type DeleteReturn<
  S extends Schema,
  M extends ModelDef,
  O extends DeleteOpts<S, M>,
> = Promise<ResultPayload<S, M, O>>;

export type DeleteManyReturn = Promise<{ count: number }>;

export type UpsertReturn<
  S extends Schema,
  M extends ModelDef,
  O extends UpsertOpts<S, M>,
> = Promise<ResultPayload<S, M, O>>;

export type UpsertManyReturn<
  S extends Schema,
  M extends ModelDef,
  O extends UpsertOpts<S, M>,
> = Promise<ResultPayload<S, M, O>[]>;

// ---------- Graph Return Types ---------- //

export type PathReturn<S extends Schema, M extends ModelDef> = Promise<{
  nodes: InferModelDef<M, S>[];
  edges: Array<{
    type: string;
    direction: "incoming" | "outgoing";
    props?: any;
  }>;
}>;

export type NeighborsReturn<S extends Schema, M extends ModelDef> = Promise<
  InferModelDef<M, S>[]
>;

export type ConnectReturn = Promise<{ success: boolean }>;

export type DisconnectReturn = Promise<{ success: boolean }>;

export type TraverseReturn<S extends Schema, M extends ModelDef> = Promise<{
  path: Array<{ node: InferModelDef<M, S>; edge?: any }>;
}>;

export class Model<S extends Schema, M extends ModelDef> {
  // ? Not sure if these will be necessary
  private $schema: S;
  private $def: M;
  private $adapter: Adapter<any>;

  constructor(schema: S, node: M, adapter: Adapter<any>) {
    this.$schema = schema;
    this.$def = node;
    this.$adapter = adapter;
  }

  async findFirst<O extends QueryOpts<S, M>>(
    opts: O
  ): FindFirstReturn<S, M, O> {
    // TODO
    console.log(opts);
    return {} as any;
  }

  async findMany<O extends QueryOpts<S, M>>(opts: O): FindManyReturn<S, M, O> {
    // TODO
    console.log(opts);
    return [];
  }

  async count<O extends QueryOpts<S, M>>(opts: O): CountReturn {
    // TODO
    console.log(opts);
    return 0;
  }

  async create<O extends CreateOpts<S, M>>(opts: O): CreateReturn<S, M, O> {
    // TODO
    console.log(opts);
    return {} as any;
  }

  async update<O extends UpdateOpts<S, M>>(opts: O): UpdateReturn<S, M, O> {
    // TODO
    console.log(opts);
    return {} as any;
  }

  async updateMany<O extends UpdateOpts<S, M>>(opts: O): UpdateManyReturn {
    // TODO
    console.log(opts);
    return { count: 0 };
  }

  async delete<O extends DeleteOpts<S, M>>(opts: O): DeleteReturn<S, M, O> {
    // TODO
    console.log(opts);
    return {} as any;
  }

  async deleteMany<O extends DeleteOpts<S, M>>(opts: O): DeleteManyReturn {
    // TODO
    console.log(opts);
    return { count: 0 };
  }

  async upsert<O extends UpsertOpts<S, M>>(opts: O): UpsertReturn<S, M, O> {
    // TODO
    console.log(opts);
    return {} as any;
  }

  async upsertMany<O extends UpsertOpts<S, M>>(
    opts: O
  ): UpsertManyReturn<S, M, O> {
    // TODO
    console.log(opts);
    return [];
  }
}

export class GraphModel<S extends Schema, M extends ModelDef> extends Model<
  S,
  M
> {
  async findPath(opts: PathOpts<S, M>): PathReturn<S, M> {
    // TODO
    console.log(opts);
    return {
      edges: [],
      nodes: [],
    };
  }

  async findShortestPath(opts: PathOpts<S, M>): PathReturn<S, M> {
    // TODO
    console.log(opts);
    return {
      edges: [],
      nodes: [],
    };
  }

  async findAllPaths(opts: PathOpts<S, M>): PathReturn<S, M> {
    // TODO
    console.log(opts);
    return {
      edges: [],
      nodes: [],
    };
  }

  async findNeighbors(opts: QueryOpts<S, M>): NeighborsReturn<S, M> {
    // TODO
    console.log(opts);
    return [];
  }

  async connect(opts: ConnectOpts<S, M>): ConnectReturn {
    // TODO
    console.log(opts);
    return { success: true };
  }

  async disconnect(opts: ConnectOpts<S, M>): ConnectReturn {
    // TODO
    console.log(opts);
    return { success: true };
  }

  async traverse(opts: TraverseOpts<S, M>): TraverseReturn<S, M> {
    // TODO
    console.log(opts);
    return {
      path: [],
    };
  }
}
