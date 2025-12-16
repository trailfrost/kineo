import {
  defineSchema,
  field,
  model,
  relation,
  type Adapter,
  type Kineo,
  type Schema,
} from "kineo";
import { createAdapterFactory } from "better-auth/adapters";
import { compile } from "./compiler";

export const betterAuthSchema = defineSchema({
  users: model("user", {
    id: field.string().id(),
    name: field.string().required(),
    email: field.string().required(),
    emailVerified: field.bool().default(false),
    image: field.string().optional(),
    createdAt: field.datetime().required(),
    updatedAt: field.datetime().required(),
  }),

  sessions: model("session", {
    id: field.string().id(),
    userId: relation.to("user").required(),
    token: field.string().required(),
    expiresAt: field.datetime().required(),
    ipAddress: field.string().optional(),
    userAgent: field.string().optional(),
    createdAt: field.datetime().required(),
    updatedAt: field.datetime().required(),
  }),

  accounts: model("account", {
    id: field.string().id(),
    userId: relation.to("user").required(),
    accountId: field.string().required(),
    providerId: field.string().required(),
    accessToken: field.string().optional(),
    refreshToken: field.string().optional(),
    accessTokenExpiresAt: field.datetime().optional(),
    refreshTokenExpiresAt: field.datetime().optional(),
    scope: field.string().optional(),
    idToken: field.string().optional(),
    password: field.string().optional(),
    createdAt: field.datetime().required(),
    updatedAt: field.datetime().required(),
  }),

  verifications: model("verification", {
    id: field.string().id(),
    identifier: field.string().required(),
    value: field.string().required(),
    expiresAt: field.datetime().required(),
    createdAt: field.datetime().required(),
    updatedAt: field.datetime().required(),
  }),
});

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
