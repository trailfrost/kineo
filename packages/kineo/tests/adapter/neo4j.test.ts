import { describe, test, expect, vi, beforeAll, afterAll } from "vitest";
import neo4j from "neo4j-driver";
import { Neo4jAdapter, auth } from "@/adapters/neo4j";
import { defineSchema, field, model } from "@/schema";

// Neo4j connection settings
const NEO4J_URL = "bolt://localhost:7687";
const NEO4J_USER = "neo4j";
const NEO4J_PASS = "password";

describe("auth()", () => {
  test("creates basic auth token", () => {
    const spy = vi.spyOn(neo4j.auth, "basic");
    const opts = {
      type: "basic",
      username: "u",
      password: "p",
      realm: "r",
    } as const;
    auth(opts);
    expect(spy).toHaveBeenCalledWith("u", "p", "r");
    spy.mockRestore();
  });

  test("creates bearer auth token", () => {
    const spy = vi.spyOn(neo4j.auth, "bearer");
    auth({ type: "bearer", token: "token123" });
    expect(spy).toHaveBeenCalledWith("token123");
    spy.mockRestore();
  });

  test("creates kerberos auth token", () => {
    const spy = vi.spyOn(neo4j.auth, "kerberos");
    auth({ type: "kerberos", ticket: "ticket123" });
    expect(spy).toHaveBeenCalledWith("ticket123");
    spy.mockRestore();
  });

  test("creates custom auth token", () => {
    const spy = vi.spyOn(neo4j.auth, "custom");
    auth({
      type: "custom",
      principal: "user",
      credentials: "cred",
      realm: "realm",
      scheme: "scheme",
      params: { extra: 1 },
    });
    expect(spy).toHaveBeenCalledWith("user", "cred", "realm", "scheme", {
      extra: 1,
    });
    spy.mockRestore();
  });
});

describe("Neo4jAdapter (integration)", () => {
  let adapter: ReturnType<typeof Neo4jAdapter>;

  beforeAll(async () => {
    adapter = Neo4jAdapter({
      url: NEO4J_URL,
      auth: {
        type: "basic",
        username: NEO4J_USER,
        password: NEO4J_PASS,
      },
    });

    // Clean database before testing
    await adapter.session.run("MATCH (n) DETACH DELETE n");
  });

  afterAll(async () => {
    await adapter.close();
  });

  test("connects and runs simple write/read queries", async () => {
    const createRes = await adapter.session.run(
      "CREATE (n:Person {name: $name, age: $age}) RETURN n",
      { name: "Alice", age: 30 },
    );

    expect(createRes.records.length).toBe(1);
    const node = createRes.records[0].get("n");
    expect(node.labels).toContain("Person");

    const result = await adapter.exec({
      command: "MATCH (n:Person) RETURN n",
      params: {},
    });

    expect(result.entries.length).toBe(1);
    const entry = result.entries[0];
    expect(entry.n.name).toBe("Alice");
    expect(entry.n.age).toBe(30);
    expect(result.summary).toBeDefined();
    expect((result.raw as any).length).toBeGreaterThan(0);
  });

  test("handles relationships correctly", async () => {
    await adapter.session.run(
      `CREATE (a:Person {name: 'Bob'})-[:KNOWS {since: 2020}]->(b:Person {name: 'Charlie'})`,
    );

    const result = await adapter.exec({
      command: "MATCH p=(a:Person)-[r:KNOWS]->(b:Person) RETURN p",
      params: {},
    });

    expect(result.edgeCount).toBeGreaterThan(0);
    const edge = result.edges![0];
    expect(edge.type).toBe("KNOWS");
    expect(edge.props.since).toBe(2020);
  });

  // ---------------------------------------------------------------------------
  // pull()
  // ---------------------------------------------------------------------------

  test("pull() extracts labels, properties, and relationships", async () => {
    await adapter.session.run(`
      CREATE (u:User {name: "Alice", age: 25})
      CREATE (p:Post {title: "Hello"})
      CREATE (u)-[:WROTE]->(p)
    `);

    const pulled = await adapter.pull!();

    expect(pulled).toBeDefined();
    expect(pulled.schema.User).toBeDefined();
    expect(pulled.schema.Post).toBeDefined();

    // Sampled properties
    expect(pulled.schema.User.name).toBeDefined();
    expect(pulled.schema.User.age).toBeDefined();

    // Sampled relationships
    expect(pulled.schema.User.WROTE).toBeDefined();
    expect(pulled.schema.Post.WROTE).toBeDefined();
  });

  // ---------------------------------------------------------------------------
  // push()
  // ---------------------------------------------------------------------------

  test("push() applies constraints & indexes without throwing", async () => {
    const schema = defineSchema({
      User: model("User", {
        id: field.string().id().required(),
        name: field.string().required(),
      }),
    });

    await expect(adapter.push!(schema)).resolves.not.toThrow();
  });

  // ---------------------------------------------------------------------------
  // generate()
  // ---------------------------------------------------------------------------

  test("generate() detects added fields and produces migration entries", async () => {
    const prev = defineSchema({
      User: model("User", {}),
    });

    const cur = defineSchema({
      User: model("User", {
        age: field.int().default(0),
      }),
    });

    const migrations = await adapter.generate!(prev, cur);

    expect(migrations.length).toBeGreaterThan(0);
    expect(
      migrations.some((m) => m.type === "command" && m.command.includes("SET")),
    ).toBe(true);
  });

  // ---------------------------------------------------------------------------
  // deploy() + status()
  // ---------------------------------------------------------------------------

  test("deploy() stores migration metadata and status() returns completed", async () => {
    const hash = "test-hash-123";
    const migration = "CREATE (:MetaTest {x: 1})";

    await adapter.deploy!(migration, hash);

    const state = await adapter.status!(migration, hash);

    expect(state).toBe("completed");
  });

  test("status() returns pending if migration hash not found", async () => {
    const state = await adapter.status!("", "non-existing-hash");
    expect(state).toBe("pending");
  });
});
