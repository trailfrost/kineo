import type { IR } from "./ir";

export type Params = Record<string, unknown>;
export type Command = { command: string; params: Params };

export type QueryRecord = Map<number, Node>;

export type QueryResult = {
  records: QueryRecord[];
};

export type OptPromise<T> = T | Promise<T>;

export type Adapter = {
  close(): OptPromise<void>;
  compile(ir: IR): OptPromise<Command>;
  run(command: string, params: Params): OptPromise<QueryResult>;
  count(command: string, params: Params): OptPromise<number>;
  getNodeLabels(): OptPromise<string[]>;
  getRelationshipTypes(): OptPromise<string[]>;
  getNodeProperties(label: string): OptPromise<string[]>;
  getRelationshipProperties(type: string): OptPromise<string[]>;
};

export class Node {}
