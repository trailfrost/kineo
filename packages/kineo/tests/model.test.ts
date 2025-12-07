import { describe, test, expect, beforeEach, vi } from "vitest";
import { Model, GraphModel } from "@/model";

// ---------- Mock IR compiler ---------- //
vi.mock("@/ir", () => ({
  compileToIR: vi.fn((name: string, op: string, opts: any) => ({
    name,
    op,
    opts,
  })),
}));

// ---------- Fake In-Memory Adapter ---------- //
function makeFakeAdapter() {
  const store: Record<string, any[]> = {};

  const adapter = {
    Model: Model as any,

    async compile(ir: any) {
      return {
        command: ir.op,
        params: ir.opts,
      };
    },

    async exec(result: { command: string; params: any }) {
      const { command, params } = result;
      const name = "User";
      const rows = store[name] ?? [];
      let entries: any[] = [];
      let entryCount = 0;

      switch (command) {
        case "findFirst":
          entries = rows.slice(0, 1);
          break;
        case "findMany":
          entries = rows;
          break;
        case "count":
          entryCount = rows.length;
          break;
        case "create": {
          const newItem = { id: rows.length + 1, ...params.data };
          store[name] = [...rows, newItem];
          entries = [newItem];
          break;
        }
        case "update": {
          const updated = rows.map((r) =>
            r.id === params.where.id ? { ...r, ...params.data } : r,
          );
          store[name] = updated;
          entries = updated.filter((r) => r.id === params.where.id);
          break;
        }
        case "updateMany": {
          const updated = rows.map((r) => ({
            ...r,
            ...(params.data ?? {}),
          }));
          store[name] = updated;
          entryCount = updated.length;
          entries = [{ count: updated.length }];
          break;
        }
        case "delete": {
          const remaining = rows.filter((r) => r.id !== params.where.id);
          const deleted = rows.find((r) => r.id === params.where.id);
          store[name] = remaining;
          entries = deleted ? [deleted] : [];
          break;
        }
        case "deleteMany": {
          const count = rows.length;
          store[name] = [];
          entries = [{ count }];
          break;
        }
        case "upsert": {
          const existing = rows.find((r) => r.id === params.where.id);
          if (existing) {
            Object.assign(existing, params.update);
            entries = [existing];
          } else {
            const newItem = { id: params.where.id, ...params.create };
            store[name] = [...rows, newItem];
            entries = [newItem];
          }
          break;
        }
        case "upsertMany": {
          const results: any[] = [];
          for (const entry of params.data ?? []) {
            const existing = rows.find((r) => r.id === entry.where.id);
            if (existing) {
              Object.assign(existing, entry.update);
              results.push(existing);
            } else {
              const newItem = { id: entry.where.id, ...entry.create };
              store[name] = [...(store[name] ?? []), newItem];
              results.push(newItem);
            }
          }
          entries = results;
          break;
        }
        // Graph operations
        case "findPath":
        case "findShortestPath":
        case "findAllPaths":
          entries = rows;
          return {
            entries,
            entryCount: rows.length,
            edges: [{ type: "REL", direction: "outgoing" }],
          };
        case "findNeighbors":
          entries = rows;
          break;
        case "connect":
        case "disconnect":
          return {
            entries: [],
            entryCount: 0,
            summary: { success: true },
          };
        case "traverse":
          return {
            entries: rows,
            entryCount: rows.length,
            edges: [{ type: "REL", direction: "outgoing" }],
          };
      }

      return { entries, entryCount };
    },

    async close() {},
  };

  return { adapter, store };
}

// ---------- Tests ---------- //
describe("Model (with fake adapter)", () => {
  let adapter: any;
  let store: Record<string, any[]>;
  let model: Model<any, any>;

  beforeEach(() => {
    ({ adapter, store } = makeFakeAdapter());
    store["User"] = [
      { id: 1, name: "Alice" },
      { id: 2, name: "Bob" },
    ];
    model = new Model("User", adapter, []);
  });

  test("findFirst returns first record", async () => {
    const result = await model.findFirst({});
    expect(result).toEqual({ id: 1, name: "Alice" });
  });

  test("findMany returns all records", async () => {
    const result = await model.findMany({});
    expect(result).toHaveLength(2);
  });

  test("count returns number of records", async () => {
    const result = await model.count({});
    expect(result).toBe(2);
  });

  test("create adds a new record", async () => {
    const result = await model.create({ data: { name: "Charlie" } });
    expect(result).toMatchObject({ id: 3, name: "Charlie" });
    expect(store["User"]).toHaveLength(3);
  });

  test("update modifies a record", async () => {
    const result = await model.update({
      where: { id: 1 },
      data: { name: "Alicia" },
    });
    expect(result).toEqual({ id: 1, name: "Alicia" });
  });

  test("updateMany updates all records and returns count", async () => {
    const result = await model.updateMany({
      where: {},
      data: { updated: true },
    });
    expect(result).toEqual([{ count: 2 }]);
    expect(store["User"].every((u) => u.updated)).toBe(true);
  });

  test("delete removes a record", async () => {
    const result = await model.delete({ where: { id: 2 } });
    expect(result).toEqual({ id: 2, name: "Bob" });
    expect(store["User"]).toHaveLength(1);
  });

  test("deleteMany removes all records", async () => {
    const result = await model.deleteMany({ where: {} });
    expect(result).toEqual([{ count: 2 }]);
    expect(store["User"]).toHaveLength(0);
  });

  test("upsert creates a record if not found", async () => {
    const result = await model.upsert({
      where: { id: 5 },
      create: { name: "Eve" },
      update: { name: "Updated" },
    });
    expect(result).toEqual({ id: 5, name: "Eve" });
    expect(store["User"]).toHaveLength(3);
  });

  test("upsert updates a record if found", async () => {
    const result = await model.upsert({
      where: { id: 1 },
      create: { name: "New" },
      update: { name: "UpdatedAlice" },
    });
    expect(result).toEqual({ id: 1, name: "UpdatedAlice" });
  });
});

describe("GraphModel (with fake adapter)", () => {
  let adapter: any;
  let store: Record<string, any[]>;
  let graph: GraphModel<any, any>;

  beforeEach(() => {
    ({ adapter, store } = makeFakeAdapter());
    store["User"] = [
      { id: 1, name: "Root" },
      { id: 2, name: "Child" },
    ];
    graph = new GraphModel("User", adapter, []);
  });

  test("findPath returns nodes and edges", async () => {
    const result = await graph.findPath({
      from: { where: { id: 1 } },
      to: { where: { id: 2 } },
    });
    expect(result.nodes).toHaveLength(2);
    expect(result.edges[0]).toMatchObject({ type: "REL" });
  });

  test("findNeighbors returns nodes", async () => {
    const result = await graph.findNeighbors({});
    expect(result).toHaveLength(2);
  });

  test("connect returns success true", async () => {
    const result = await graph.connect({
      from: { where: { id: 1 } },
      to: { where: { id: 2 } },
      relation: "LINKS_TO",
    });
    expect(result).toEqual({ success: true });
  });

  test("traverse returns path with nodes and edges", async () => {
    const result = await graph.traverse({ start: { where: { id: 1 } } });
    expect(result.path).toHaveLength(2);
    expect(result.path[0]).toHaveProperty("node");
    expect(result.path[0]).toHaveProperty("edge");
  });
});
