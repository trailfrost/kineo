import * as neo4j from "neo4j-driver";
import type * as Neo4j from "neo4j-driver";
import type { Node, Schema } from "./schema";

export type Auth =
  | {
      type?: "basic";
      username: string;
      password: string;
      realm?: string;
    }
  | {
      type: "bearer";
      token: string;
    }
  | {
      type: "kerberos";
      ticket: string;
    }
  | {
      type: "custom";
      principal: string;
      credentials: string;
      realm: string;
      scheme: string;
      parameters: Record<string, unknown>;
    };

export type DatabaseOpts<TSchema extends Schema> = (
  | {
      url: string;
      auth: Auth;
    }
  | {
      driver: Neo4j.Driver;
    }
) & {
  schema: TSchema;
  driverConfig?: Neo4j.Config;
};

export type KineoOGM<TSchema extends Schema> = {
  driver: Neo4j.Driver;
} & { [node in keyof TSchema]: Model<TSchema[node]> };

export class Model<TNode extends Node> {
  node: TNode;

  constructor(node: TNode) {
    this.node = node;
  }
}

export function createDriver(
  url: string,
  auth: Auth,
  config?: Neo4j.Config
): Neo4j.Driver {
  let authToken: Neo4j.AuthToken;
  switch (auth.type) {
    case "basic":
    case undefined:
      authToken = neo4j.auth.basic(auth.username, auth.password);
      break;
    case "bearer":
      authToken = neo4j.auth.bearer(auth.token);
      break;
    case "kerberos":
      authToken = neo4j.auth.kerberos(auth.ticket);
      break;
    case "custom":
      authToken = neo4j.auth.custom(
        auth.principal,
        auth.credentials,
        auth.realm,
        auth.scheme,
        auth.parameters
      );
      break;
  }

  return neo4j.driver(url, authToken, config);
}

export function Kineo<TSchema extends Schema>(
  opts: DatabaseOpts<TSchema>
): KineoOGM<TSchema> {
  const record: {
    [node in keyof TSchema]?: Model<TSchema[node]>;
  } = {};

  for (const key in opts.schema) {
    record[key] = new Model(opts.schema[key]);
  }

  const driver =
    "driver" in opts
      ? opts.driver
      : createDriver(opts.url, opts.auth, opts.driverConfig);
  return {
    ...record,
    driver,
  } as KineoOGM<TSchema>;
}

export * from "./schema";
