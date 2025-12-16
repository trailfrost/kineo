import type { Adapter, Kineo, Schema } from "kineo";
import { createAdapterFactory } from "better-auth/adapters";
import { createSchema } from "./schema";
import { compile } from "./compiler";

export const kineoAdapter = (client: Kineo<any, any>) =>
  createAdapterFactory({
    config: {
      adapterId: "@kineojs/better-auth",
    },
    adapter: () => ({
      async count(props) {
        return (await exec(client, "count", props)).entryCount;
      },

      async create(props) {
        return (await exec(client, "create", props)).entries[0];
      },

      async delete(props) {
        await exec(client, "delete", props);
      },

      async deleteMany(props) {
        return (await exec(client, "deleteMany", props)).entryCount;
      },

      async findOne(props) {
        return (await exec(client, "findOne", props)).entries[0];
      },

      async findMany(props) {
        return (await exec(client, "findMany", props)).entries;
      },

      async update(props) {
        return (await exec(client, "update", props)).entries[0];
      },

      async updateMany(props) {
        return (await exec(client, "updateMany", props)).entryCount;
      },

      createSchema,
    }),
  });

async function exec(
  client: Kineo<Schema, Adapter<any, any>>,
  mode: string,
  props: any,
) {
  const ir = compile(mode, props);
  const result = await client.$adapter.compile(ir);
  return (await client.$adapter.exec(result)) as any;
}
