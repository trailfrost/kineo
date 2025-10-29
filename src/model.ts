import type { Adapter } from "./adapter";
import type { Direction, InferModelDef, ModelDef, Schema } from "./schema";

// ---------- Generic Utility Types ---------- //

/**
 * Filter interface for primitive types.
 * @param T The primitive type.
 */
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

/**
 * Filter for a field.
 * @param T The field's type.
 */
export type FieldFilter<T> = T | PrimitiveFilter<T>;

/**
 * Extracts ID from a model.
 */
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

/**
 * Query options (`findFirst`, `findMany`, etc.)
 * @param S The schema.
 * @param M The model definition.
 * @param MType _Do not pass_ Inferred type of the model definition.
 */
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

/**
 * Create options (`create`).
 * @param S The schema.
 * @param M The model definition.
 * @param MType _Do not pass_ Inferred type of the model definition.
 */
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

/**
 * Update options (`update`, `updateMany`).
 * @param S The schema.
 * @param M The model definition.
 * @param MType _Do not pass_ Inferred type of the model definition.
 */
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

/**
 * Delete options (`delete`, `deleteMany`).
 * @param S The schema.
 * @param M The model definition.
 * @param MType _Do not pass_ Inferred type of the model definition.
 */
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

/**
 * Upsert options (`upsert`, `upsertMany`).
 * @param S The schema.
 * @param M The model definition.
 * @param MType _Do not pass_ Inferred type of the model definition.
 */
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

/**
 * Path options (`findPath`, `findShortestPath`, etc.).
 * @param S The schema.
 * @param M The model definition.
 * @param MType _Do not pass_ Inferred type of the model definition.
 */
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

/**
 * Connect options (`connect`, `disconnect`).
 * @param S The schema.
 * @param M The model definition.
 * @param MType _Do not pass_ Inferred type of the model definition.
 */
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

/**
 * Traverse options (`traverse`).
 * @param S The schema.
 * @param M The model definition.
 * @param MType _Do not pass_ Inferred type of the model definition.
 */
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

/**
 * If select/include specified, infer shape, otherwise return full model.
 * @param S The schema.
 * @param M The model definition.
 * @param O The options passed into the function.
 * @param MType _Do not pass_ The inferred type of the model definition.
 */
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
/**
 * Picks a subset of the fields, based on the `select` option.
 * @param S The schema.
 * @param M The model definition.
 * @param Sel The fields to select.
 * @param MType _Do not pass_ The inferred type from the model definition.
 */
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

/**
 * Adds nested relations.
 * @param S The schema.
 * @param M The model definition.
 * @param Inc The included relations.
 * @param MType _Do not pass_ The inferred type from the model definition.
 */
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

/**
 * The return type for `findFirst`.
 */
export type FindFirstReturn<
  S extends Schema,
  M extends ModelDef,
  O extends QueryOpts<S, M> | undefined,
> = Promise<ResultPayload<S, M, O> | null>;

/**
 * The return type for `findMany`.
 */
export type FindManyReturn<
  S extends Schema,
  M extends ModelDef,
  O extends QueryOpts<S, M> | undefined,
> = Promise<ResultPayload<S, M, O>[]>;

/**
 * The return type for `count`.
 */
export type CountReturn = Promise<number>;

/**
 * The return type for `create`.
 */
export type CreateReturn<
  S extends Schema,
  M extends ModelDef,
  O extends CreateOpts<S, M>,
> = Promise<ResultPayload<S, M, O>>;

/**
 * The return  type for `update`.
 */
export type UpdateReturn<
  S extends Schema,
  M extends ModelDef,
  O extends UpdateOpts<S, M>,
> = Promise<ResultPayload<S, M, O>>;

/**
 * The return type for `updateMany`.
 */
// Make this return the whole record if you want
export type UpdateManyReturn = Promise<{ count: number }>;

/**
 * The return type for `delete`.
 */
export type DeleteReturn<
  S extends Schema,
  M extends ModelDef,
  O extends DeleteOpts<S, M>,
> = Promise<ResultPayload<S, M, O>>;

/**
 * The return type for `deleteMany`.
 */
// Make this return the whole record if you want
export type DeleteManyReturn = Promise<{ count: number }>;

/**
 * The return type for `upsert`.
 */
export type UpsertReturn<
  S extends Schema,
  M extends ModelDef,
  O extends UpsertOpts<S, M>,
> = Promise<ResultPayload<S, M, O>>;

/**
 * The return type for `upsertMany`.
 */
export type UpsertManyReturn<
  S extends Schema,
  M extends ModelDef,
  O extends UpsertOpts<S, M>,
> = Promise<ResultPayload<S, M, O>[]>;

// ---------- Graph Return Types ---------- //

/**
 * The return type for everything related to paths.
 */
export type PathReturn<S extends Schema, M extends ModelDef> = Promise<{
  nodes: InferModelDef<M, S>[];
  edges: Array<{
    type: string;
    direction: "incoming" | "outgoing";
    props?: any;
  }>;
}>;

/**
 * The return  type for `findNeighbors`.
 */
export type NeighborsReturn<S extends Schema, M extends ModelDef> = Promise<
  InferModelDef<M, S>[]
>;

/**
 * The return  type for `connect`.
 */
export type ConnectReturn = Promise<{ success: boolean }>;

/**
 * The return  type for `disconnect`.
 */
export type DisconnectReturn = Promise<{ success: boolean }>;

/**
 * The return  type for `traverse`.
 */
export type TraverseReturn<S extends Schema, M extends ModelDef> = Promise<{
  path: Array<{ node: InferModelDef<M, S>; edge?: any }>;
}>;

/**
 * A model. This is different from a model definition; the definition is just the schema, the class provides the functionality.
 * @param S The schema.
 * @param M The model definition.
 */
export class Model<S extends Schema, M extends ModelDef> {
  // ? Not sure if these will be necessary
  /**
   * The schema.
   */
  private $schema: S;
  /**
   * The definition.
   */
  private $def: M;
  /**
   * The adapter.
   */
  private $adapter: Adapter<any>;

  /**
   * Creates a new model. This is usually done by Kineo -- it is not recommended to create a model manually like this.
   * @param schema The schema.
   * @param node The model definition.
   * @param adapter The adapter.
   */
  constructor(schema: S, node: M, adapter: Adapter<any>) {
    this.$schema = schema;
    this.$def = node;
    this.$adapter = adapter;
  }

  /**
   * Finds the first element matching a filter.
   * @param opts Query options.
   * @returns The first element that matches the filter, or `null` if not found.
   */
  async findFirst<O extends QueryOpts<S, M>>(
    opts: O
  ): FindFirstReturn<S, M, O> {
    // TODO
    console.log(opts);
    return {} as any;
  }

  /**
   * Finds a list of elements matching a filter.
   * @param opts Query options.
   * @returns The elements that match the filter.
   */
  async findMany<O extends QueryOpts<S, M>>(opts: O): FindManyReturn<S, M, O> {
    // TODO
    console.log(opts);
    return [];
  }

  /**
   * Counts the number of elements matching a filter.
   * @param opts Query options.
   * @returns The first element that matches the filter, or `null` if not found.
   */
  async count<O extends QueryOpts<S, M>>(opts: O): CountReturn {
    // TODO
    console.log(opts);
    return 0;
  }

  /**
   * Creates an element.
   * @param opts Create options.
   * @returns The just created element.
   */
  async create<O extends CreateOpts<S, M>>(opts: O): CreateReturn<S, M, O> {
    // TODO
    console.log(opts);
    return {} as any;
  }

  /**
   * Updates an element.
   * @param opts Update options.
   * @returns The element that was updated.
   */
  async update<O extends UpdateOpts<S, M>>(opts: O): UpdateReturn<S, M, O> {
    // TODO
    console.log(opts);
    return {} as any;
  }

  /**
   * Updates multiple elements.
   * @param opts Update options.
   * @returns The amount of elements that were updated.
   */
  async updateMany<O extends UpdateOpts<S, M>>(opts: O): UpdateManyReturn {
    // TODO
    console.log(opts);
    return { count: 0 };
  }

  /**
   * Deletes an element.
   * @param opts Delete options.
   * @returns The element that was deleted.
   */
  async delete<O extends DeleteOpts<S, M>>(opts: O): DeleteReturn<S, M, O> {
    // TODO
    console.log(opts);
    return {} as any;
  }

  /**
   * Deletes multiple elements.
   * @param opts Delete options.
   * @returns The amount of elements that were deleted.
   */
  async deleteMany<O extends DeleteOpts<S, M>>(opts: O): DeleteManyReturn {
    // TODO
    console.log(opts);
    return { count: 0 };
  }

  /**
   * Upserts (updates if it exists, creates otherwise) an element.
   * @param opts Upsert options.
   * @returns The element that was upserted.
   */
  async upsert<O extends UpsertOpts<S, M>>(opts: O): UpsertReturn<S, M, O> {
    // TODO
    console.log(opts);
    return {} as any;
  }

  /**
   * Upserts (updates if it exists, creates otherwise) multiple elements.
   * @param opts Upsert options.
   * @returns The elements that were upserted.
   */
  async upsertMany<O extends UpsertOpts<S, M>>(
    opts: O
  ): UpsertManyReturn<S, M, O> {
    // TODO
    console.log(opts);
    return [];
  }
}

/**
 * Provides utility methods for graph databases on top of the default model.
 * @param S The schema.
 * @param M The model definition.
 */
export class GraphModel<S extends Schema, M extends ModelDef> extends Model<
  S,
  M
> {
  /**
   * Finds a path that matches a filter.
   * @param opts Path options.
   * @returns The path the matches the filter.
   */
  async findPath(opts: PathOpts<S, M>): PathReturn<S, M> {
    // TODO
    console.log(opts);
    return {
      edges: [],
      nodes: [],
    };
  }

  /**
   * Finds the shortest path to a node.
   * @param opts Path options.
   * @returns The shortest path.
   */
  async findShortestPath(opts: PathOpts<S, M>): PathReturn<S, M> {
    // TODO
    console.log(opts);
    return {
      edges: [],
      nodes: [],
    };
  }

  /**
   * Finds all paths that match a filter.
   * @param opts Path options.
   * @returns The paths that match the filter.
   */
  async findAllPaths(opts: PathOpts<S, M>): PathReturn<S, M> {
    // TODO
    console.log(opts);
    return {
      edges: [],
      nodes: [],
    };
  }

  /**
   * Finds neighbor nodes, or nodes that are connected directly.
   * @param opts Query options.
   * @returns The neighbor nodes.
   */
  async findNeighbors(opts: QueryOpts<S, M>): NeighborsReturn<S, M> {
    // TODO
    console.log(opts);
    return [];
  }

  /**
   * Connects a node to another node.
   * @param opts Connect options.
   * @returns If the connection was successful or not.
   */
  async connect(opts: ConnectOpts<S, M>): ConnectReturn {
    // TODO
    console.log(opts);
    return { success: true };
  }

  /**
   * Disconnects a node from another node.
   * @param opts Connect options.
   * @returns If the connection was successful or not.
   */
  async disconnect(opts: ConnectOpts<S, M>): ConnectReturn {
    // TODO
    console.log(opts);
    return { success: true };
  }

  /**
   * Traverses a graph.
   * @param opts Traverse options.
   * @returns The paths it passed through.
   */
  async traverse(opts: TraverseOpts<S, M>): TraverseReturn<S, M> {
    // TODO
    console.log(opts);
    return {
      path: [],
    };
  }
}
