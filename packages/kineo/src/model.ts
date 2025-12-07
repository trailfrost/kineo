import type { Adapter } from "./adapter";
import type { Direction, InferModelDef, ModelDef, Schema } from "./schema";
import * as ir from "@/ir";

// ---------- Generic Utility Types ---------- //

/**
 * Filter interface for primitive types.
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

// ---------- Result Type Helpers ---------- //

/**
 * If select/include specified, infer shape, otherwise return full model.
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

/**
 * A model. This is different from a model definition; the definition is just the schema, the class provides the functionality.
 */
export class Model<S extends Schema, M extends ModelDef> {
  /**
   * The name of the model.
   */
  protected $name: string;
  /**
   * The adapter.
   */
  protected $adapter: Adapter<any, any>;

  /**
   * Creates a new model. This is usually done by Kineo -- it is not recommended to create a model manually like this.
   * @param name The name of the model.
   * @param adapter The adapter.
   */
  constructor(name: string, adapter: Adapter<any, any>) {
    this.$name = name;
    this.$adapter = adapter;
  }

  protected async $exec(opts: any, op: string) {
    const tree = ir.compileToIR(this.$name, op, opts);
    const compiled = await this.$adapter.compile(tree);
    const result = await this.$adapter.exec(compiled);

    return result;
  }

  /**
   * Finds the first element matching a filter.
   * @param opts Query options.
   * @returns The first element that matches the filter, or `null` if not found.
   */
  async findFirst<O extends QueryOpts<S, M>>(
    opts: O,
  ): FindFirstReturn<S, M, O> {
    const { entries: rows } = await this.$exec(opts, "findFirst");
    return (rows[0] ?? null) as any;
  }

  /**
   * Finds a list of elements matching a filter.
   * @param opts Query options.
   * @returns The elements that match the filter.
   */
  async findMany<O extends QueryOpts<S, M>>(opts: O): FindManyReturn<S, M, O> {
    const { entries: rows } = await this.$exec(opts, "findMany");
    return rows as any;
  }

  /**
   * Counts the number of elements matching a filter.
   * @param opts Query options.
   * @returns The first element that matches the filter, or `null` if not found.
   */
  async count<O extends QueryOpts<S, M>>(opts: O): CountReturn {
    const { entryCount: rowCount } = await this.$exec(opts, "count");
    return rowCount;
  }

  /**
   * Creates an element.
   * @param opts Create options.
   * @returns The just created element.
   */
  async create<O extends CreateOpts<S, M>>(opts: O): CreateReturn<S, M, O> {
    const { entries: rows } = await this.$exec(opts, "create");
    return (rows[0] ?? null) as any;
  }

  /**
   * Updates an element.
   * @param opts Update options.
   * @returns The element that was updated.
   */
  async update<O extends UpdateOpts<S, M>>(opts: O): UpdateReturn<S, M, O> {
    const { entries: rows } = await this.$exec(opts, "update");
    return (rows[0] ?? null) as any;
  }

  /**
   * Updates multiple elements.
   * @param opts Update options.
   * @returns The amount of elements that were updated.
   */
  async updateMany<O extends UpdateOpts<S, M>>(opts: O): UpdateManyReturn {
    const { entries: rows } = await this.$exec(opts, "updateMany");
    return rows as any;
  }

  /**
   * Deletes an element.
   * @param opts Delete options.
   * @returns The element that was deleted.
   */
  async delete<O extends DeleteOpts<S, M>>(opts: O): DeleteReturn<S, M, O> {
    const { entries: rows } = await this.$exec(opts, "delete");
    return (rows[0] ?? null) as any;
  }

  /**
   * Deletes multiple elements.
   * @param opts Delete options.
   * @returns The amount of elements that were deleted.
   */
  async deleteMany<O extends DeleteOpts<S, M>>(opts: O): DeleteManyReturn {
    const { entries: rows } = await this.$exec(opts, "deleteMany");
    return rows as any;
  }

  /**
   * Upserts (updates if it exists, creates otherwise) an element.
   * @param opts Upsert options.
   * @returns The element that was upserted.
   */
  async upsert<O extends UpsertOpts<S, M>>(opts: O): UpsertReturn<S, M, O> {
    const { entries: rows } = await this.$exec(opts, "upsert");
    return (rows[0] ?? null) as any;
  }

  /**
   * Upserts (updates if it exists, creates otherwise) multiple elements.
   * @param opts Upsert options.
   * @returns The elements that were upserted.
   */
  async upsertMany<O extends UpsertOpts<S, M>>(
    opts: O,
  ): UpsertManyReturn<S, M, O> {
    const { entries: rows } = await this.$exec(opts, "upsertMany");
    return rows as any;
  }
}

// ---------- Graph-specific Options ---------- //

/**
 * Path options (`findPath`, `findShortestPath`, etc.).
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
 * Traverse options (`traverse`)..
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
 * Provides utility methods for graph databases on top of the default model.
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
    const result = await this.$exec(opts, "findPath");
    return {
      nodes: result.entries as InferModelDef<M, S>[],
      edges: result.edges ?? [],
    };
  }

  /**
   * Finds the shortest path to a node.
   * @param opts Path options.
   * @returns The shortest path.
   */
  async findShortestPath(opts: PathOpts<S, M>): PathReturn<S, M> {
    const result = await this.$exec(opts, "findShortestPath");
    return {
      nodes: result.entries as InferModelDef<M, S>[],
      edges: result.edges ?? [],
    };
  }

  /**
   * Finds all paths that match a filter.
   * @param opts Path options.
   * @returns The paths that match the filter.
   */
  async findAllPaths(opts: PathOpts<S, M>): PathReturn<S, M> {
    const result = await this.$exec(opts, "findAllPaths");
    return {
      nodes: result.entries as InferModelDef<M, S>[],
      edges: result.edges ?? [],
    };
  }

  /**
   * Finds neighbor nodes, or nodes that are connected directly.
   * @param opts Query options.
   * @returns The neighbor nodes.
   */
  async findNeighbors(opts: QueryOpts<S, M>): NeighborsReturn<S, M> {
    const result = await this.$exec(opts, "findNeighbors");
    return result.entries as InferModelDef<M, S>[];
  }

  /**
   * Connects a node to another node.
   * @param opts Connect options.
   * @returns If the connection was successful or not.
   */
  async connect(opts: ConnectOpts<S, M>): ConnectReturn {
    const result = await this.$exec(opts, "connect");
    return { success: !!result.summary || true };
  }

  /**
   * Disconnects a node from another node.
   * @param opts Connect options.
   * @returns If the connection was successful or not.
   */
  async disconnect(opts: ConnectOpts<S, M>): DisconnectReturn {
    const result = await this.$exec(opts, "disconnect");
    return { success: !!result.summary || true };
  }

  /**
   * Traverses a graph.
   * @param opts Traverse options.
   * @returns The paths it passed through.
   */
  async traverse(opts: TraverseOpts<S, M>): TraverseReturn<S, M> {
    const result = await this.$exec(opts, "traverse");

    return {
      path: (result.entries ?? []).map((node, i) => ({
        node: node as InferModelDef<M, S>,
        edge: result.edges?.[i],
      })),
    };
  }
}
