import type { Adapter, Params } from "kineo/adapter";
import type {
  Config,
  Driver,
  AuthToken,
  Session,
  ServerInfo,
} from "neo4j-driver";
import neo4j from "neo4j-driver";
import compile from "./compiler";

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
        config.auth.parameters
      );
      break;
  }

  return neo4j.driver(config.url, authToken, config);
}

export type Kineo4j = Adapter & {
  driver: Driver;
  session: Session;
  serverInfo?: ServerInfo;
};

export default function Neo4jAdapter(config: AdapterConfig): Kineo4j {
  const driver = createDriver(config);
  const session = driver.session();

  return {
    driver,
    session,

    schemaIntrospection: [], // TODO

    async run(command, params) {
      const result = await session.run(command, params);
      return {
        records: result.records.map((record) => record.get(0)),
      };
    },

    compile(ir) {
      return compile(ir);
    },

    // TODO remove functions below

    async getRelationshipTypes() {
      const result = await session.run(`CALL db.relationshipTypes()`);
      return result.records.map((r) => r.get("relationshipType"));
    },

    async getNodeProperties(label: string) {
      if (!this.serverInfo) this.serverInfo = await driver.getServerInfo();
      const result = await session.run(
        `
        CALL db.schema.nodeTypeProperties()
        YIELD nodeType, propertyName
        WHERE $label = nodeType
        RETURN DISTINCT propertyName
        `,
        { label }
      );
      return result.records.map((r) => r.get("propertyName"));
    },

    async getRelationshipProperties(type: string) {
      const result = await session.run(
        `
        CALL db.schema.relTypeProperties()
        YIELD relType, propertyName
        WHERE relType = $type
        RETURN DISTINCT propertyName
        `,
        { type }
      );
      return result.records.map((r) => r.get("propertyName"));
    },

    async count(command: string, params: Params) {
      const countQuery = `CALL { ${command} } RETURN count(n) as count`;
      const result = await session.run(countQuery, params);
      return result.records[0].get("count")?.toInt?.() || 0;
    },

    async getNodeLabels() {
      const result = await session.run(`CALL db.labels()`);
      return result.records.map((r) => r.get("label"));
    },

    async close() {
      await session.close();
      await driver.close();
    },
  };
}
