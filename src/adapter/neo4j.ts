import type { Adapter } from ".";
import { GraphModel } from "@/model";
import * as neo4j from "neo4j-driver";

export type Auth =
  | {
      type: "basic";
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
      params: Record<string, any>;
    };

export type Neo4jOpts =
  | {
      driver: neo4j.Driver;
      session?: neo4j.Session | neo4j.SessionConfig;
    }
  | {
      url: string;
      auth: Auth;
      session?: neo4j.SessionConfig;
    };

export interface Neo4jAdapter
  extends Adapter<typeof GraphModel, neo4j.ResultSummary> {
  driver: neo4j.Driver;
  session: neo4j.Session;
}

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
      console.log(ir);
      // TODO
      return { command: "", params: {} };
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
