import type { Adapter } from ".";
import { GraphModel } from "@/model";
import { compile } from "@/compiler/cypher";
import * as neo4j from "neo4j-driver";

/**
 * Authentication options for Neo4j.
 */
export type Auth =
  | {
      /**
       * Basic (username and password) authentication.
       */
      type: "basic";
      /**
       * The username.
       */
      username: string;
      /**
       * The password.
       */
      password: string;
      /**
       * An optional realm.
       */
      realm?: string;
    }
  | {
      /**
       * Bearer token authentication.
       */
      type: "bearer";
      /**
       * The token.
       */
      token: string;
    }
  | {
      /**
       * Kerberos ticket authentication.
       */
      type: "kerberos";
      /**
       * The ticket.
       */
      ticket: string;
    }
  | {
      /**
       * Customized authentication.
       */
      type: "custom";
      /**
       * The username.
       */
      principal: string;
      /**
       * The credentials.
       */
      credentials: string;
      /**
       * The realm of the authentication.
       */
      realm: string;
      /**
       * The scheme (`basic`, `bearer`, etc.)
       */
      scheme: string;
      /**
       * Optional parameters.
       */
      params: Record<string, any>;
    };

export type Neo4jOpts =
  | {
      /**
       * The driver.
       */
      driver: neo4j.Driver;
      /**
       * The session.
       */
      session?: neo4j.Session | neo4j.SessionConfig;
    }
  | {
      /**
       * The URL of your database.
       */
      url: string;
      /**
       * Authentication options.
       */
      auth: Auth;
      /**
       * Configuration for a Neo4j session.
       */
      session?: neo4j.SessionConfig;
    };

/**
 * A Neo4j adapter.
 */
export interface Neo4jAdapter
  extends Adapter<typeof GraphModel, neo4j.ResultSummary> {
  /**
   * The driver.
   */
  driver: neo4j.Driver;
  /**
   * The session.
   */
  session: neo4j.Session;
}

/**
 * Creates a new Neo4j adapter.
 * @param opts Options for creating the adapter.
 * @returns A Neo4j adapter.
 */
export function Neo4jAdapter(opts: Neo4jOpts): Neo4jAdapter {
  const driver =
    "driver" in opts ? opts.driver : neo4j.driver(opts.url, auth(opts.auth));
  const session =
    opts.session instanceof neo4j.Session
      ? opts.session
      : typeof opts.session === "undefined"
        ? driver.session()
        : driver.session(opts.session);
  return {
    Model: GraphModel,
    async close() {
      await session.close();
      await driver.close();
    },

    compile(ir) {
      return compile(ir);
    },

    async exec(result) {
      const { records, summary } = await session.run(
        result.command,
        result.params
      );

      const entries: any[] = [];
      const edges: any[] = [];

      for (const record of records) {
        const obj: Record<string, any> = {};

        for (const key of record.keys) {
          const value = toNative(record.get(key));
          obj[key.toString()] = value;

          // Collect relationship-like objects
          collectEdges(value, edges);
        }

        entries.push(obj);
      }

      return {
        entries,
        entryCount: entries.length,
        edges,
        edgeCount: edges.length,
        summary,
        raw: records,
      };
    },

    driver,
    session,
  };
}

/**
 * Converts a Neo4j value to a vanilla JavaScript type.
 * @param value The value to convert.
 * @returns The converted value.
 */
function toNative(value: any): any {
  if (neo4j.isInt(value)) {
    // convert neo4j.Integer -> number (safe)
    return value.inSafeRange() ? value.toNumber() : value.toBigInt();
  }

  if (value instanceof neo4j.Node) {
    // Return node properties with an optional id & labels
    return {
      identity: toNative(value.identity),
      labels: value.labels,
      ...toNative(value.properties),
    };
  }

  if (value instanceof neo4j.Relationship) {
    // Return relationship properties with id, start, end
    return {
      identity: toNative(value.identity),
      start: toNative(value.start),
      end: toNative(value.end),
      type: value.type,
      ...toNative(value.properties),
    };
  }

  if (value instanceof neo4j.Path) {
    // Flatten paths to their nodes and relationships
    return {
      start: toNative(value.start),
      end: toNative(value.end),
      segments: value.segments.map((seg) => ({
        start: toNative(seg.start),
        relationship: toNative(seg.relationship),
        end: toNative(seg.end),
      })),
    };
  }

  if (Array.isArray(value)) {
    return value.map(toNative);
  }

  if (value && typeof value === "object") {
    const obj: Record<string, any> = {};
    for (const [k, v] of Object.entries(value)) obj[k] = toNative(v);
    return obj;
  }

  return value;
}

/**
 * Collects edges/relationship-like objects.
 * @param value The value.
 * @param edges The array to collect to.
 */
function collectEdges(value: any, edges: any[]) {
  if (!value) return;

  // Relationship-like object (produced by toNative)
  if (value.type && "start" in value && "end" in value && "identity" in value) {
    edges.push({
      id: value.identity,
      type: value.type,
      start: value.start,
      end: value.end,
      props: Object.fromEntries(
        Object.entries(value).filter(
          ([k]) => !["identity", "type", "start", "end"].includes(k)
        )
      ),
    });
  }

  // Path-like object
  if (Array.isArray(value?.segments)) {
    for (const seg of value.segments) {
      collectEdges(seg.relationship, edges);
    }
  }

  // Recurse into arrays/objects
  if (Array.isArray(value)) {
    for (const v of value) collectEdges(v, edges);
  } else if (typeof value === "object") {
    for (const v of Object.values(value)) collectEdges(v, edges);
  }
}

/**
 * Creates an authentication token.
 * @param opts The authentication options.
 * @returns An authentication token.
 */
function auth(opts: Auth) {
  switch (opts.type) {
    case "basic":
      return neo4j.auth.basic(opts.username, opts.password, opts.realm);
    case "bearer":
      return neo4j.auth.bearer(opts.token);
    case "kerberos":
      return neo4j.auth.kerberos(opts.ticket);
    case "custom":
      return neo4j.auth.custom(
        opts.principal,
        opts.credentials,
        opts.realm,
        opts.scheme,
        opts.params
      );
  }
}
