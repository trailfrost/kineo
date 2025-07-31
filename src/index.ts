import * as neo4j from "neo4j-driver";
import type * as Neo4j from "neo4j-driver";
import type { InferSchema, Schema } from "./schema";
import Model from "./model";

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

type ModelsForSchema<TSchema extends Schema> = {
  [Node in keyof TSchema]: Model<TSchema, TSchema[Node]>;
};

export type KineoOGM<TSchema extends Schema> = KineoClient<TSchema> &
  ModelsForSchema<TSchema>;

export type InferClient<T> =
  T extends KineoOGM<infer TSchema> ? InferSchema<TSchema> : never;

class KineoClient<TSchema extends Schema> {
  driver: Neo4j.Driver;
  session: Neo4j.Session;

  constructor(opts: DatabaseOpts<TSchema>) {
    if ("driver" in opts) {
      this.driver = opts.driver;
    } else {
      this.driver = KineoClient.createDriver(
        opts.url,
        opts.auth,
        opts.driverConfig
      );
    }

    this.session = this.driver.session();

    // Assign all models to `this`
    for (const key in opts.schema) {
      const model = new Model(key, opts.schema, opts.schema[key], this.session);
      (this as unknown as Record<string, unknown>)[key] = model;
    }
  }

  static createDriver(
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

  async close(): Promise<void> {
    await this.session.close();
    await this.driver.close();
  }

  async transaction(): Promise<Neo4j.Transaction> {
    return await this.session.beginTransaction();
  }
  async cypher<Shape extends neo4j.RecordShape>(
    command: string,
    params?: Record<string, unknown>
  ) {
    return await this.session.run<Shape>(command, params);
  }
}

export default function Kineo<TSchema extends Schema>(
  opts: DatabaseOpts<TSchema>
): KineoOGM<TSchema> {
  return new KineoClient(opts) as KineoOGM<TSchema>;
}
