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
      const rows = records.map((record) => {
        const obj: Record<string, any> = {};
        for (const key of record.keys) {
          obj[key.toString()] = toNative(record.get(key));
        }
        return obj;
      });

      return {
        rows,
        summary,
        rowCount: rows.length,
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
    return value.inSafeRange() ? value.toNumber() : value.toString();
  }

  if (value instanceof neo4j.types.Node) {
    // Return node properties with an optional id & labels
    return {
      identity: toNative(value.identity),
      labels: value.labels,
      ...toNative(value.properties),
    };
  }

  if (value instanceof neo4j.types.Relationship) {
    // Return relationship properties with id, start, end
    return {
      identity: toNative(value.identity),
      start: toNative(value.start),
      end: toNative(value.end),
      type: value.type,
      ...toNative(value.properties),
    };
  }

  if (value instanceof neo4j.types.Path) {
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
