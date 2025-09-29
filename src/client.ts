import type { InferSchema, Schema } from "./schema.js";
import type { Adapter } from "./adapter.js";
import { Model } from "./model.js";

// Mapped type over a schema that defines model types
type ModelsForSchema<TSchema extends Schema, TAdapter extends Adapter<any>> = {
  [Key in keyof TSchema]: Key extends string
    ? TAdapter extends Adapter<infer TExtends>
      ? Model<TSchema, any> & TExtends
      : never
    : never;
};

/**
 * A Kineo client.
 * @param TSchema The schema to generate models from.
 * @param TAdapter The adapter used in this client.
 */
export type KineoClient<
  TSchema extends Schema,
  TAdapter extends Adapter<any>,
> = ModelsForSchema<TSchema, TAdapter> & {
  /**
   * The adapter.
   */
  $adapter: TAdapter;
  /**
   * The schema.
   */
  $schema: TSchema;
};

/**
 * Infers a schema from a client.
 * @param T The client to infer types frmom.
 */
export type InferClient<T> =
  T extends KineoClient<infer TSchema, any> ? InferSchema<TSchema> : never;

/**
 * Creates a Kineo client.
 * @param schema The schema.
 * @returns A Kineo client.
 */
export function Kineo<TAdapter extends Adapter<any>, TSchema extends Schema>(
  adapter: TAdapter,
  schema: TSchema
): KineoClient<TSchema, TAdapter> {
  const client: Record<string, Model<any, any>> = {};
  for (const key in schema) {
    client[key] = new Model(schema, schema[key], adapter);
    adapter.extendModel(client[key]);
  }

  return {
    ...client,
    $adapter: adapter,
    $schema: schema,
  } as KineoClient<TSchema, TAdapter>;
}
