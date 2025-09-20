import Model, { type GetTargetNodeType, type RelationshipKeys } from "../model";
import type { Schema, Node, InferNode } from "../schema";
import type { Adapter } from "../adapter";
import { parseConnect, parseDisconnect } from "./ir";

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

export default class GraphModel<
  S extends Schema,
  N extends Node,
  A extends Adapter<any>,
> extends Model<S, N, A> {
  async connect(opts: ConnectOpts<S, N>) {
    const ir = parseConnect(this.label, "n", opts, this.node);
    const result = await this.run(ir);
    return this.toNodeProperties(result.records[0]);
  }

  async disconnect(opts: ConnectOpts<S, N>) {
    const ir = parseDisconnect(this.label, "n", opts, this.node);
    const result = await this.run(ir);
    return this.toNodeProperties(result.records[0]);
  }
}
