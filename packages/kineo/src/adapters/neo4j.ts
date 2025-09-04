import type { Adapter } from "../adapter";
import type { Config, Driver, AuthToken, Session } from "neo4j-driver";
import neo4j from "neo4j-driver";
import compile from "../compilers/cypher";

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

export type AdapterConfig =
  | ({
      url: string;
      auth: Auth;
    } & Config)
  | {
      driver: Driver;
    };

/**
 * Creates a new Neo4j driver.
 * @param url URL to your database.
 * @param auth Authentication options.
 * @param config Driver configuration.
 * @returns A Neo4j driver.
 */
function createDriver(config: AdapterConfig): Driver {
  if ("driver" in config) return config.driver;

  let authToken: AuthToken;

  switch (config.auth.type) {
    case "basic":
    case undefined:
      authToken = neo4j.auth.basic(config.auth.username, config.auth.password);
      break;
    case "bearer":
      authToken = neo4j.auth.bearer(config.auth.token);
      break;
    case "kerberos":
      authToken = neo4j.auth.kerberos(config.auth.ticket);
      break;
    case "custom":
      authToken = neo4j.auth.custom(
        config.auth.principal,
        config.auth.credentials,
        config.auth.realm,
        config.auth.scheme,
        config.auth.parameters,
      );
      break;
  }

  return neo4j.driver(config.url, authToken, config);
}

/**
 * Type for Kineo Neo4j adapter.
 */
export type Kineo4j = Adapter & {
  driver: Driver;
  session: Session;
};

/**
 * Creates a Neo4j adapter for Kineo.
 * @param config The configuration for the adapter.
 * @returns A Neo4j adapter.
 */
export default function Neo4jAdapter(config: AdapterConfig): Kineo4j {
  const driver = createDriver(config);
  const session = driver.session();

  return {
    driver,
    session,

    compile,

    async getSchema() {
      return {}; // TODO
    },

    async close() {
      await session.close();
      await driver.close();
    },

    async run(command, params) {
      const result = await session.run(command, params);
      return {
        records: result.records.map((record) => record.get(0)),
      };
    },

    async push(schema) {
      throw new Error("This adapter does not support pushing."); // TODO
    },

    async status(migrations, hashes) {
      throw new Error("This adapter does not support migrations."); // TODO
    },

    async deploy(migration, hash) {
      throw new Error("This adapter does not support migrations."); // TODO
    },

    async migrate(diff) {
      throw new Error("This adapter does not support migrations."); // TODO
    },
  };
}
