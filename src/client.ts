import { InferSchema, Schema } from "./schema.js";
import { Model } from "./model.js";

type ModelsForSchema<TSchema extends Schema> = {
  [Key in keyof TSchema]: Key extends string ? Model : never;
};

type KineoClient<TSchema extends Schema> = ModelsForSchema<TSchema> & {
  schema: TSchema;
};

export type InferClient<T> =
  T extends KineoClient<infer TSchema> ? InferSchema<TSchema> : never;

export function Kineo<T extends Schema>(schema: T): KineoClient<T> {
  const client: Record<string, Model> = {};
  for (const key in schema) {
    client[key] = new Model(); // TODO
  }

  return {
    ...client,
    schema,
  } as KineoClient<T>;
}
