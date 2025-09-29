import type { InferSchema, Schema } from "./schema.js";
import type { Model, GraphModel } from "./model.js";
import type { Adapter } from "./adapter.js";

// Mapped type over a schema that defines model types
type ModelsForSchema<TSchema extends Schema, TAdapter extends Adapter<any>> = {
  [Key in keyof TSchema]: Key extends string
    ? TAdapter extends Adapter<infer TModel>
      ? TModel extends GraphModel<any, any>
        ? GraphModel<TSchema, TSchema[Key]>
        : Model<TSchema, TSchema[Key]>
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
    client[key] = new adapter.Model(schema, schema[key], adapter);
  }

  return {
    ...client,
    $adapter: adapter,
    $schema: schema,
  } as KineoClient<TSchema, TAdapter>;
}
