import * as neo4j from "neo4j-driver";
import type * as Neo4j from "neo4j-driver";
import type { InferSchema, Schema } from "./schema";
import Model from "./model";

/**
 * Neo4j authentication options.
 */
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

/**
 * Options for creating a database.
 */
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

/**
 * Utility type for creating models from a schema.
 */
type ModelsForSchema<TSchema extends Schema> = {
  [Node in keyof TSchema]: Model<TSchema, TSchema[Node]>;
};

/**
 * Kineo Client + all the models from a schema.
 */
export type KineoOGM<TSchema extends Schema> = KineoClient<TSchema> &
  ModelsForSchema<TSchema>;

/**
 * Infers the TypeScript types from a Kineo OGM.
 */
export type InferClient<T> =
  T extends KineoOGM<infer TSchema> ? InferSchema<TSchema> : never;

/**
 * Kineo client.
 */
class KineoClient<TSchema extends Schema> {
  /**
   * The driver of this client.
   */
  driver: Neo4j.Driver;
  /**
   * The session to run queries on.
   */
  session: Neo4j.Session;

  /**
   * Creates a new Kineo client.
   * @param opts Options for connecting to the database.
   */
  constructor(opts: DatabaseOpts<TSchema>) {
    if ("driver" in opts) {
      this.driver = opts.driver;
    } else {
      this.driver = KineoClient.createDriver(
        opts.url,
        opts.auth,
        opts.driverConfig,
      );
    }

    this.session = this.driver.session();

    // Assign all models to `this`
    for (const key in opts.schema) {
      const model = new Model(key, opts.schema, opts.schema[key], this.session);
      (this as unknown as Record<string, unknown>)[key] = model;
    }
  }

  /**
   * Creates a new Neo4j driver.
   * @param url URL to your database.
   * @param auth Authentication options.
   * @param config Driver configuration.
   * @returns A Neo4j driver.
   */
  static createDriver(
    url: string,
    auth: Auth,
    config?: Neo4j.Config,
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
          auth.parameters,
        );
        break;
    }

    return neo4j.driver(url, authToken, config);
  }

  /**
   * Closes the session and driver.
   */
  async close(): Promise<void> {
    await this.session.close();
    await this.driver.close();
  }

  /**
   * Creates a new transaction.
   * @returns A new transaction.
   */
  async transaction(): Promise<Neo4j.Transaction> {
    return await this.session.beginTransaction();
  }

  /**
   * Runs Cypher directly on the session.
   * @param command The Cypher command to execute. To put variables in here, don't use a template literal - instead, use `$name`, then pass a key of `name` into the parameters.
   * @param params Parameters.
   * @returns A list of records and raw Cypher.
   */
  async cypher<Shape extends neo4j.RecordShape>(
    command: string,
    params?: Record<string, unknown>,
  ) {
    return await this.session.run<Shape>(command, params);
  }
}

/**
 * Creates a new Kineo OGM client.
 * @param opts Options for connecting to the database.
 * @returns A new Kineo OGM client.
 */
export default function Kineo<TSchema extends Schema>(
  opts: DatabaseOpts<TSchema>,
): KineoOGM<TSchema> {
  return new KineoClient(opts) as KineoOGM<TSchema>;
}
