import type { InferSchema, Schema } from "./schema.js";
import type { Model } from "./model.js";
import type { Adapter } from "./adapter.js";

// Mapped type over a schema that defines model types
type ModelsForSchema<
  TSchema extends Schema,
  TAdapter extends Adapter<Model>,
> = {
  [Key in keyof TSchema]: Key extends string
    ? TAdapter extends Adapter<infer TModel>
      ? TModel
      : never
    : never;
};

/**
 * A Kineo client.
 */
export type KineoClient<
  TSchema extends Schema,
  TAdapter extends Adapter<any>,
> = ModelsForSchema<TSchema, TAdapter> & {
  /**
   * The schema.
   */
  schema: TSchema;
};

/**
 * Infers a schema from a client.
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
  const client: Record<string, Model> = {};
  for (const key in schema) {
    client[key] = new adapter.Model(); // TODO
  }

  return {
    ...client,
    schema,
  } as KineoClient<TSchema, TAdapter>;
}
