import {
  defineSchema,
  field,
  model,
  relation,
  type InferSchema,
} from "kineo/schema";
import Kineo from "kineo";
import config from "../kineo.config";
import { Adapter } from "kineo/adapter";

export const schema = defineSchema({
  User: model({
    name: field.string().id(),
    bio: field.string().required(),
    posts: relation.to("posts").outgoing("HAS_POSTS").array().default([]),
  }),

  Post: model({
    id: field.string().id(),
    title: field.string().required(),
    author: relation.to("users").incoming("HAS_POSTS").required(),
  }),
});

export type Schema = InferSchema<typeof schema>;

export const adapter: Adapter = {
  status(migrations, hashes) {
    console.log(migrations, hashes);
    return ["deployed", "pending"];
  },

  close() {
    console.log("Closing!");
  },

  compile(ir) {
    console.log(ir);
    return { command: "SELECT * from table", params: {} };
  },

  deploy(migration, hash) {
    console.log(migration, hash);
  },

  getSchema() {
    return {};
  },

  migrate(diff) {
    console.log(diff);
    return ["CREATE TABLE name IF NOT EXISTS"];
  },

  push(schema) {
    console.log(schema);
  },

  run(command, params) {
    console.log(command, params);
    return {
      records: [],
    };
  },
};

export const client = Kineo(adapter, schema);

export { config };
