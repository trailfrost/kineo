import type { Schema, Node, InferNode, RelationshipDef } from "./schema";
import type { QueryResult, QueryRecord, Node as AdapterNode } from "./adapter";
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
import { Adapter } from "./adapter";

/**
 * Utility to extract relationship definitions.
 */
export type RelationshipKeys<N extends Node> = {
  // eslint-disable-next-line
  [K in keyof N]: N[K] extends RelationshipDef<any> ? K : never;
}[keyof N];

/**
 * Gets a relationship target.
 */
export type GetRelationshipTargetLabel<N extends Node, R extends keyof N> =
  N[R] extends RelationshipDef<infer To> ? To : never;

/**
 * Gets the type of a relationship target.
 */
export type GetTargetNodeType<
  S extends Schema,
  N extends Node,
  R extends RelationshipKeys<N>,
> = S[GetRelationshipTargetLabel<N, R>] extends Node
  ? InferNode<S[GetRelationshipTargetLabel<N, R>], S>
  : never;

/**
 * Options for connecting two nodes.
 */
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

/**
 * Options for checking if a node is connected.
 */
export type IsConnectedOpts<
  S extends Schema,
  N extends Node,
  R extends RelationshipKeys<N> = RelationshipKeys<N>,
> = ConnectOpts<S, N, R>;

/**
 * Options for upserting a connection.
 */
export type UpsertConnectOpts<
  S extends Schema,
  N extends Node,
  R extends RelationshipKeys<N> = RelationshipKeys<N>,
> = ConnectOpts<S, N, R> & {
  create?: boolean;
};

/**
 * Options for deleting a connection.
 */
export type DeleteConnectionOpts<
  S extends Schema,
  N extends Node,
  R extends RelationshipKeys<N> = RelationshipKeys<N>,
> = ConnectOpts<S, N, R>;

/**
 * Options for getting a relationship.
 */
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

/**
 * Defines a field in a `where` clause.
 */
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

/**
 * A `WHERE` node.
 */
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

/**
 * A `WHERE` clause.
 */
export type Where<S extends Schema, N extends Node> = WhereNode<
  InferNode<N, S>
>;

/**
 * Options for simple queries.
 */
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

/**
 * Options for creating a node.
 */
export type CreateOpts<
  S extends Schema,
  N extends Node,
  INode = InferNode<N, S>,
> = {
  data: INode;
};

/**
 * Options for merging (creating or updating) a node.
 */
export type MergeOpts<
  S extends Schema,
  N extends Node,
  INode = InferNode<N, S>,
> = {
  where: Partial<INode>;
  update?: Partial<INode>;
  create?: INode;
};

/**
 * Options for deleting a node.
 */
export type DeleteOpts<
  S extends Schema,
  N extends Node,
  INode = InferNode<N, S>,
> = {
  where?: Partial<INode>;
};

/**
 * Applies default values.
 * @param nodeDef The node to apply to.
 * @param record The record to apply to.
 * @returns The record, with defaults applied.
 */
function applyDefaults<N extends Node, S extends Schema>(
  nodeDef: N,
  record: Record<string, unknown>
) {
  for (const key in nodeDef) {
    const def = nodeDef[key];
    const hasValue = record[key as string] !== undefined;

    if (!hasValue) {
      if ("defaultValue" in def && def.defaultValue !== undefined) {
        record[key] = def.defaultValue;
      } else if (
        "isArray" in def &&
        def.isArray === true &&
        def.defaultValue === undefined
      ) {
        // fallback to [] if array with no default explicitly set
        record[key] = [];
      }
    }
  }
  return record as InferNode<N, S>;
}

/**
 * A model, created when instantiating the OGM. Provides functions for interacting with the database.
 */
export default class Model<
  S extends Schema,
  N extends Node,
  A extends Adapter,
> {
  /**
   * The label of the node this model applies to.
   */
  readonly label: string;
  /**
   * Full schema.
   */
  readonly schema: S;
  /**
   * Individual node this applies to.
   */
  readonly node: N;
  /**
   * Session to run commands in.
   */
  readonly adapter: A;

  /**
   * Creates a new model.
   * @param label The label of the node this model applies to.
   * @param schema Full schema.
   * @param node Node this model applies to.
   * @param session Session to run commands in.
   */
  constructor(label: string, schema: S, node: N, adapter: A) {
    this.label = label;
    this.schema = schema;
    this.node = node;
    this.adapter = adapter;
  }

  /**
   * Compiles and runs an IR.
   * @param ir The IR to compile and run.
   * @returns The result of running the IR.
   */
  private async run(ir: IRStatement): Promise<QueryResult> {
    const { command, params } = await this.adapter.compile({
      statements: [ir],
    });
    return await this.adapter.run(command, params);
  }

  /**
   * Gets the properties from a record.
   * @param record The record.
   */
  private toNodeProperties(record: QueryRecord): InferNode<N, S> | null;
  /**
   * Gets all properties from a list of records.
   * @param record The records.
   */
  private toNodeProperties(record: QueryRecord[]): InferNode<N, S>[];

  private toNodeProperties(record: QueryRecord | QueryRecord[]) {
    if (!record) return null;

    const apply = (rec: QueryRecord) => {
      const node = "get" in rec ? rec.get(0)! : (rec as AdapterNode);
      if (!("properties" in node)) {
        return null;
      }
      return applyDefaults<N, S>(this.node, node.properties as InferNode<N, S>);
    };

    if (Array.isArray(record)) return record.map(apply);
    return apply(record);
  }

  /**
   * Runs a `MATCH` query and returns a single node.
   * @param opts Match options.
   * @returns Properties of the matched node.
   */
  async matchOne(opts?: QueryOpts<S, N>) {
    const ir = parseMatch(this.label, "n", opts ?? {});
    const result = await this.run(ir);
    return this.toNodeProperties(result.records[0]);
  }

  /**
   * Runs a `MATCH` query and returns multiple nodes.
   * @param opts Match options.
   * @returns Properties of the matched nodes.
   */
  async matchMany(opts?: QueryOpts<S, N>) {
    const ir = parseMatch(this.label, "n", opts ?? {});
    const result = await this.run(ir);
    return this.toNodeProperties(result.records);
  }

  /**
   * Runs a `MATCH` query to check if a node exists.
   * @param opts Match options.
   * @returns If the node exists.
   */
  async exists(opts: Where<S, N>) {
    const ir = parseMatch(this.label, "n", { where: opts, limit: 1 });
    const result = await this.run(ir);
    return result.records.length > 0;
  }

  /**
   * Runs a `CREATE` query and returns one node.
   * @param opts Create options.
   * @returns Properties of the created node.
   */
  async createOne(opts: CreateOpts<S, N>) {
    const ir = parseCreate(this.label, "n", opts);
    const result = await this.run(ir);
    return this.toNodeProperties(result.records[0]);
  }

  /**
   * Runs a `CREATE` query and returns multiple nodes.
   * @param opts Create options.
   * @returns Properties of the created nodes.
   */
  async createMany(opts: CreateOpts<S, N>) {
    const ir = parseCreate(this.label, "n", opts);
    const result = await this.run(ir);
    return this.toNodeProperties(result.records);
  }

  /**
   * Runs a `MERGE` query and returns one node.
   * @param opts Create options.
   * @returns Properties of the merged node.
   */
  async mergeOne(opts: MergeOpts<S, N>) {
    const ir = parseMerge(this.label, "n", opts);
    const result = await this.run(ir);
    return this.toNodeProperties(result.records[0]);
  }

  /**
   * Runs a `MERGE` query and returns multiple node.
   * @param opts Create options.
   * @returns Properties of the merged nodes.
   */
  async mergeMany(opts: MergeOpts<S, N>) {
    const ir = parseMerge(this.label, "n", opts);
    const result = await this.run(ir);
    return this.toNodeProperties(result.records);
  }

  /**
   * Runs a `DELETE` query and returns one node.
   * @param opts Create options.
   * @returns Properties of the deleted node.
   */
  async deleteOne(opts: DeleteOpts<S, N>) {
    const ir = parseDelete(this.label, "n", opts);
    const result = await this.run(ir);
    return this.toNodeProperties(result.records[0]);
  }

  /**
   * Runs a `DELETE` query and returns multiple node.
   * @param opts Create options.
   * @returns Properties of the deleted nodes.
   */
  async deleteMany(opts?: DeleteOpts<S, N>) {
    const ir = parseDelete(this.label, "n", opts ?? {});
    const result = await this.run(ir);
    return this.toNodeProperties(result.records);
  }

  /**
   * Connects two nodes together.
   * @param opts Connection options.
   * @returns The first node.
   */
  async connect(opts: ConnectOpts<S, N>) {
    const ir = parseConnect(this.label, "n", opts, this.node);
    const result = await this.run(ir);
    return this.toNodeProperties(result.records[0]);
  }

  /**
   * Disconnects two nodes.
   * @param opts Disconnection options.
   * @returns The first node.
   */
  async disconnect(opts: ConnectOpts<S, N>) {
    const ir = parseDisconnect(this.label, "n", opts, this.node);
    const result = await this.run(ir);
    return this.toNodeProperties(result.records[0]);
  }

  /**
   * Checks if two nodes are connected.
   * @param opts Connection options.
   * @returns The first node.
   */
  async isConnected(opts: IsConnectedOpts<S, N>) {
    const ir = parseConnect(this.label, "n", opts, this.node);
    const result = await this.run(ir);
    return result.records.length > 0;
  }

  /**
   * Upserts a relationship.
   * @param opts Connection options.
   * @returns The first node.
   */
  async upsertRelation(opts: UpsertConnectOpts<S, N>) {
    const ir = parseConnect(this.label, "n", opts, this.node); // Could differentiate based on `create?: true`
    const result = await this.run(ir);
    return this.toNodeProperties(result.records[0]);
  }

  /**
   * Removes a relationship.
   * @param opts Connection options.
   * @returns The first node.
   */
  async deleteRelation(opts: DeleteConnectionOpts<S, N>) {
    const ir = parseDisconnect(this.label, "n", opts, this.node);
    const result = await this.run(ir);
    return this.toNodeProperties(result.records[0]);
  }

  /**
   * Gets all relationships.
   * @param opts What relations to get
   * @returns The relations.
   */
  async getRelations(opts: GetRelationOpts<S, N>) {
    const ir = parseRelationQuery(this.schema, this.label, opts);
    const result = await this.run(ir);
    return this.toNodeProperties(
      result.records
    ) as unknown as GetTargetNodeType<S, N, RelationshipKeys<N>>[];
  }

  /**
   * Counts nodes.
   * @param opts Nodes to count.
   * @returns Number of nodes that match.
   */
  async count(opts?: Where<S, N>) {
    const ir = parseMatch(this.label, "n", {
      where: opts,
      select: {}, // just match the node, then count
    });
    const { command, params } = await this.adapter.compile({
      statements: [ir],
    });
    const result = await this.adapter.count(command, params);
    return result;
  }

  /**
   * Gets all labels.
   * @returns Labels.
   */
  async getNodeLabels(): Promise<string[]> {
    return await this.adapter.getNodeLabels();
  }

  /**
   * Gets all relationship types.
   * @returns Types.
   */
  async getRelationshipTypes(): Promise<string[]> {
    return await this.adapter.getRelationshipTypes();
  }

  /**
   * Gets properties of a node.
   * @param label The label to get properties of.
   * @param arrayType Only enable this if node types are arrays.
   * @returns Node properties.
   */
  async getNodeProperties(label: string): Promise<string[]> {
    return await this.adapter.getNodeProperties(label);
  }

  /**
   * Gets relationship properties.
   * @param type The relationship type.
   * @returns Relationship properties.
   */
  async getRelationshipProperties(type: string): Promise<string[]> {
    return await this.adapter.getRelationshipProperties(type);
  }
}
