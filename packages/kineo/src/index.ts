import type { InferSchema, Schema } from "./schema";
import type { Adapter } from "./adapter";
import Model from "./model";

/**
 * Utility type for creating models from a schema.
 */
type ModelsForSchema<TSchema extends Schema, TAdapter extends Adapter<any>> = {
  [K in keyof TSchema]: Model<TSchema, TSchema[K], TAdapter>;
};

/**
 * Kineo Client, including all the models from a schema.
 */
export type KineoClient<
  TSchema extends Schema,
  TAdapter extends Adapter<any>,
> = {
  adapter: TAdapter;
  close(): void | Promise<void>;
} & ModelsForSchema<TSchema, TAdapter>;

/**
 * Infers the TypeScript types from a Kineo OGM.
 */
export type InferClient<T> =
  T extends KineoClient<infer TSchema, any> ? InferSchema<TSchema> : never;

/**
 * Creates a new Kineo client.
 * @param adapter The database adapter.
 * @param schema Schema for building the Kineo client.
 * @returns A new Kineo client.
 */
export default function Kineo<
  TSchema extends Schema,
  TAdapter extends Adapter<any>,
>(adapter: TAdapter, schema: TSchema): KineoClient<TSchema, TAdapter> {
  const modelsForSchema = {} as ModelsForSchema<TSchema, TAdapter>;
  for (const label in schema) {
    const node = schema[label];
    modelsForSchema[label] = new adapter.Model(
      label,
      schema,
      node,
      adapter
    ) as any;
  }

  return Object.assign(
    {
      adapter,
      close: adapter.close,
    },
    modelsForSchema
  );
}
