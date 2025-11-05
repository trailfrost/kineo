import type { InferSchema, Schema } from "./schema";
import type { Model, GraphModel } from "./model";
import type { Adapter } from "./adapter";

// Mapped type over a schema that defines model types
type ModelsForSchema<
  TSchema extends Schema,
  TAdapter extends Adapter<any, any>,
> = {
  [Key in keyof TSchema]: Key extends string
    ? TAdapter extends Adapter<infer TModelCtor, any>
      ? InstanceType<TModelCtor> extends GraphModel<any, any>
        ? GraphModel<TSchema, TSchema[Key]>
        : Model<TSchema, TSchema[Key]>
      : never
    : never;
};

/**
 * A Kineo client.
 */
export type Kineo<
  TSchema extends Schema,
  TAdapter extends Adapter<any, any>,
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
 */
export type InferClient<T> =
  T extends Kineo<infer TSchema, any> ? InferSchema<TSchema> : never;

/**
 * Creates a Kineo client.
 * @param schema The schema.
 * @returns A Kineo client.
 */
export function Kineo<
  TAdapter extends Adapter<any, any>,
  TSchema extends Schema,
>(adapter: TAdapter, schema: TSchema): Kineo<TSchema, TAdapter> {
  const client: Record<string, Model<any, any>> = {};
  for (const key in schema) {
    client[key] = new adapter.Model(schema, schema[key], adapter);
  }

  return {
    ...client,
    $adapter: adapter,
    $schema: schema,
  } as Kineo<TSchema, TAdapter>;
}
