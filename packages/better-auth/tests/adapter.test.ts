import { describe, test, expect, vi, beforeEach } from "vitest";
import { kineoAdapter } from "@/index";

// mock compiler and schema
vi.mock("@/compiler", () => ({
  compile: vi.fn(),
}));

vi.mock("@/schema", () => ({
  createSchema: vi.fn(),
}));

import { compile } from "@/compiler";

describe("kineoAdapter", () => {
  let client: any;
  let execResult: any;

  beforeEach(() => {
    execResult = {
      entryCount: 5,
      entries: [{ id: 1 }, { id: 2 }],
    };

    client = {
      $adapter: {
        compile: vi.fn().mockResolvedValue("compiled-ir"),
        exec: vi.fn().mockResolvedValue(execResult),
      },
    };

    (compile as any).mockReturnValue("ir");
  });

  test("creates adapter with correct adapterId", () => {
    const factory = kineoAdapter(client);
    expect(factory({}).id).toBe("@kineojs/better-auth");
  });

  test("count returns entryCount", async () => {
    const adapter = kineoAdapter(client)({});
    const result = await adapter.count({ where: [], model: "user" });

    expect(compile).toHaveBeenCalledWith("count", {
      model: "user",
      where: [],
    });

    expect(client.$adapter.compile).toHaveBeenCalledWith("ir");
    expect(client.$adapter.exec).toHaveBeenCalledWith("compiled-ir");
    expect(result).toBe(5);
  });

  test("create returns first entry", async () => {
    const adapter = kineoAdapter(client)({});
    const result = await adapter.create({ model: "user", data: {} });

    expect(compile).toHaveBeenCalledWith(
      "create",
      expect.objectContaining({
        model: "user",
        data: expect.objectContaining({
          id: expect.any(String),
          createdAt: expect.any(Date),
          updatedAt: expect.any(Date),
        }),
      }),
    );

    expect(result).toEqual(
      expect.objectContaining({
        id: "1",
      }),
    );
  });

  test("delete returns first entry", async () => {
    const adapter = kineoAdapter(client)({});

    await adapter.delete({
      model: "user",
      where: [{ field: "id", value: 1 }],
    });

    expect(compile).toHaveBeenLastCalledWith("delete", {
      model: "user",
      where: [
        {
          field: "id",
          value: 1,
          operator: "eq",
          connector: "AND",
        },
      ],
    });
  });

  test("deleteMany returns entries", async () => {
    const adapter = kineoAdapter(client)({});

    const result = await adapter.deleteMany({
      model: "user",
      where: [],
    });

    expect(compile).toHaveBeenLastCalledWith("deleteMany", {
      model: "user",
      where: [],
    });

    expect(result).toEqual(execResult.entryCount);
  });

  test("findOne returns first entry", async () => {
    const adapter = kineoAdapter(client)({});

    const result = await adapter.findOne({
      model: "user",
      where: [{ field: "id", value: 1 }],
    });

    expect(compile).toHaveBeenLastCalledWith("findOne", {
      model: "user",
      where: [
        {
          field: "id",
          value: 1,
          operator: "eq",
          connector: "AND",
        },
      ],
    });

    expect(result).toMatchObject({
      id: "1",
    });
  });

  test("findMany returns entries", async () => {
    const adapter = kineoAdapter(client)({});

    const result = await adapter.findMany({
      model: "user",
      where: [],
    });

    expect(compile).toHaveBeenLastCalledWith("findMany", {
      model: "user",
      where: [],
      limit: 100,
      offset: undefined,
      sortBy: undefined,
      join: undefined,
    });

    expect(result).toEqual([
      expect.objectContaining({ id: "1" }),
      expect.objectContaining({ id: "2" }),
    ]);
  });

  test("update returns first entry", async () => {
    const adapter = kineoAdapter(client)({});

    const result = await adapter.update({
      model: "user",
      where: [{ field: "id", value: 1 }],
      update: {},
    });

    expect(compile).toHaveBeenLastCalledWith(
      "update",
      expect.objectContaining({
        model: "user",
        where: [
          {
            field: "id",
            value: 1,
            operator: "eq",
            connector: "AND",
          },
        ],
      }),
    );

    expect(result).toMatchObject({
      id: "1",
    });
  });

  test("updateMany returns entries", async () => {
    const adapter = kineoAdapter(client)({});

    const result = await adapter.updateMany({
      model: "user",
      where: [],
      update: {},
    });

    expect(compile).toHaveBeenLastCalledWith(
      "updateMany",
      expect.objectContaining({
        model: "user",
        where: [],
        update: expect.any(Object),
      }),
    );

    expect(result).toEqual(execResult.entryCount);
  });
});
