import type {
  Schema,
  Node as SchemaNode,
  InferNode,
  RelationshipDef,
} from "./schema";
import type { QueryResult, QueryRecord, Node as AdapterNode } from "./adapter";
import {
  parseMatch,
  parseCreate,
  parseMerge,
  parseDelete,
  type IRStatement,
} from "./ir";
import type { Adapter } from "./adapter";

/**
 * Utility to extract relationship definitions.
 */
export type RelationshipKeys<N extends SchemaNode> = {
  // eslint-disable-next-line
  [K in keyof N]: N[K] extends RelationshipDef<any> ? K : never;
}[keyof N];

/**
 * Gets a relationship target.
 */
export type GetRelationshipTargetLabel<
  N extends SchemaNode,
  R extends keyof N,
> = N[R] extends RelationshipDef<infer To> ? To : never;

/**
 * Gets the type of a relationship target.
 */
export type GetTargetNodeType<
  S extends Schema,
  N extends SchemaNode,
  R extends RelationshipKeys<N>,
> = S[GetRelationshipTargetLabel<N, R>] extends SchemaNode
  ? InferNode<S[GetRelationshipTargetLabel<N, R>], S>
  : never;

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
export type Where<S extends Schema, N extends SchemaNode> = WhereNode<
  InferNode<N, S>
>;

/**
 * Options for simple queries.
 */
export interface QueryOpts<
  S extends Schema,
  N extends SchemaNode,
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
  N extends SchemaNode,
  INode = InferNode<N, S>,
> = {
  data: INode;
};

/**
 * Options for merging (creating or updating) a node.
 */
export type MergeOpts<
  S extends Schema,
  N extends SchemaNode,
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
  N extends SchemaNode,
  INode = InferNode<N, S>,
> = {
  where?: Partial<INode>;
};

/**
 * A model, created when instantiating the OGM. Provides functions for interacting with the database.
 */
export default class Model<
  S extends Schema,
  N extends SchemaNode,
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

  protected applyDefaults<N extends SchemaNode, S extends Schema>(
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
   * Compiles and runs an IR.
   * @param ir The IR to compile and run.
   * @returns The result of running the IR.
   */
  protected async run(ir: IRStatement): Promise<QueryResult> {
    const { command, params } = await this.adapter.compile({
      statements: [ir],
    });
    return await this.adapter.run(command, params);
  }

  /**
   * Gets the properties from a record.
   * @param record The record.
   */
  protected toNodeProperties(record: QueryRecord): InferNode<N, S> | null;
  /**
   * Gets all properties from a list of records.
   * @param record The records.
   */
  protected toNodeProperties(record: QueryRecord[]): InferNode<N, S>[];

  protected toNodeProperties(record: QueryRecord | QueryRecord[]) {
    if (!record) return null;

    const apply = (rec: QueryRecord) => {
      const node = "get" in rec ? rec.get(0)! : (rec as AdapterNode);
      if (typeof node !== "object") return node;

      if (!("properties" in node)) {
        return null;
      }
      return this.applyDefaults<N, S>(
        this.node,
        node.properties as InferNode<N, S>
      );
    };

    if (Array.isArray(record)) return record.map(apply);
    return apply(record);
  }

  /**
   * A find query that returns a single node.
   * @param opts Match options.
   * @returns Properties of the matched node.
   */
  async findOne(opts?: QueryOpts<S, N>) {
    const ir = parseMatch(this.label, "n", opts ?? {});
    const result = await this.run(ir);
    return this.toNodeProperties(result.records[0]);
  }

  /**
   * A find query that returns multiple nodes.
   * @param opts Match options.
   * @returns Properties of the matched nodes.
   */
  async findMany(opts?: QueryOpts<S, N>) {
    const ir = parseMatch(this.label, "n", opts ?? {});
    const result = await this.run(ir);
    return this.toNodeProperties(result.records);
  }

  /**
   * Checks if a node exists.
   * @param opts Match options.
   * @returns If the node exists.
   */
  async exists(opts: Where<S, N>) {
    const ir = parseMatch(this.label, "n", { where: opts, limit: 1 });
    const result = await this.run(ir);
    return result.records.length > 0;
  }

  /**
   * Creates a single node.
   * @param opts Create options.
   * @returns Properties of the created node.
   */
  async createOne(opts: CreateOpts<S, N>) {
    const ir = parseCreate(this.label, "n", opts);
    const result = await this.run(ir);
    return this.toNodeProperties(result.records[0]);
  }

  /**
   * Creates multiple nodes.
   * @param opts Create options.
   * @returns Properties of the created nodes.
   */
  async createMany(opts: CreateOpts<S, N>) {
    const ir = parseCreate(this.label, "n", opts);
    const result = await this.run(ir);
    return this.toNodeProperties(result.records);
  }

  /**
   * An upsert query that returns one node.
   * @param opts Upsert options.
   * @returns Properties of the merged node.
   */
  async upsertOne(opts: MergeOpts<S, N>) {
    const ir = parseMerge(this.label, "n", opts);
    const result = await this.run(ir);
    return this.toNodeProperties(result.records[0]);
  }

  /**
   * An upsert query that returns multiple node.
   * @param opts Create options.
   * @returns Properties of the merged nodes.
   */
  async upsertMany(opts: MergeOpts<S, N>) {
    const ir = parseMerge(this.label, "n", opts);
    const result = await this.run(ir);
    return this.toNodeProperties(result.records);
  }

  /**
   * A delete query that returns one node.
   * @param opts Create options.
   * @returns Properties of the deleted node.
   */
  async deleteOne(opts: DeleteOpts<S, N>) {
    const ir = parseDelete(this.label, "n", opts);
    const result = await this.run(ir);
    return this.toNodeProperties(result.records[0]);
  }

  /**
   * A delete query that returns multiple node.
   * @param opts Create options.
   * @returns Properties of the deleted nodes.
   */
  async deleteMany(opts?: DeleteOpts<S, N>) {
    const ir = parseDelete(this.label, "n", opts ?? {});
    const result = await this.run(ir);
    return this.toNodeProperties(result.records);
  }

  /**
   * Counts nodes.
   * @param opts Nodes to count.
   * @returns Number of nodes that match.
   */
  async count(opts?: Where<S, N>): Promise<number> {
    return await this.findMany({
      where: opts,
      select: {},
    }).then((result) => result.length);
  }
}

/**
 * Node properties.
 */
type Properties = Record<string, unknown>;

/**
 * A node.
 */
export class Node {
  /**
   * Identity of node.
   */
  identity: number;
  /**
   * Labels associated with the node.
   */
  labels: string[];
  /**
   * Node properties.
   */
  properties: Properties;
  /**
   * Node element ID.
   */
  elementId: string;

  /**
   * Creates a new node.
   * @param identity The node identity.
   * @param labels The node labels.
   * @param properties The node properties.
   * @param elementId The node element ID.
   */
  constructor(
    identity: number,
    labels: string[],
    properties: Properties,
    elementId?: string
  ) {
    this.identity = identity;
    this.labels = labels;
    this.properties = properties;
    this.elementId = elementId || labels[0];
  }
}

/**
 * A relationship.
 */
export class Relationship {
  /**
   * Identity of relationship.
   */
  identity: number;
  /**
   * Start node identity.
   */
  start: number;
  /**
   * End node identity.
   */
  end: number;
  /**
   * Relationship type.
   */
  type: string;
  /**
   * Relationship properties.
   */
  properties: Properties;
  /**
   * Relationship element ID.
   */
  elementId: string;
  /**
   * Element ID of start node.
   */
  startNodeElementId: string;
  /**
   * Element ID of end node.
   */
  endNodeElementId: string;

  /**
   *
   * @param identity Identity of relationship.
   * @param start Start node identity.
   * @param end End node identity.
   * @param type Relationship type.
   * @param properties Relationship properties.
   * @param elementId Relationship element ID.
   * @param startNodeElementId Element ID of start node.
   * @param endNodeElementId Element ID of end node.
   */
  constructor(
    identity: number,
    start: number,
    end: number,
    type: string,
    properties: Properties,
    elementId: string,
    startNodeElementId: string,
    endNodeElementId: string
  ) {
    this.identity = identity;
    this.start = start;
    this.end = end;
    this.type = type;
    this.properties = properties;
    this.elementId = elementId;
    this.startNodeElementId = startNodeElementId;
    this.endNodeElementId = endNodeElementId;
  }
}
