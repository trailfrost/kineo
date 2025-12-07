import type { InferSchema, Schema } from "./schema";
import type { Model, GraphModel } from "./model";
import type { Adapter } from "./adapter";
import type { Plugin } from "./plugin";

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
  /**
   * Enables a plugin.
   */
  $plugin(plugin: Plugin): void;
};

/**
 * Infers a schema from a client.
 */
export type InferClient<T> =
  T extends Kineo<infer TSchema, any> ? InferSchema<TSchema> : never;

/**
 * Creates a Kineo client, with an adapter.
 * @param adapter The adapter.
 * @param schema The schema.
 * @returns A Kineo client.
 */
export function Kineo<
  TAdapter extends Adapter<any, any>,
  TSchema extends Schema,
>(adapter: TAdapter, schema: TSchema): Kineo<TSchema, TAdapter>;

/**
 * Creates a Kineo client, with an adapter and plugins.
 * @param plugins The plugins, with the first needing to be an adapter.
 * @param schema The schema.
 * @returns A Kineo client.
 */
export function Kineo<
  TAdapter extends Adapter<any, any>,
  TSchema extends Schema,
>(plugins: [TAdapter, ...Plugin[]], schema: TSchema): Kineo<TSchema, TAdapter>;

export function Kineo<
  TAdapter extends Adapter<any, any>,
  TSchema extends Schema,
>(adapterOrPlugins: TAdapter | [TAdapter, ...Plugin[]], schema: TSchema) {
  const isPluginArray = Array.isArray(adapterOrPlugins);
  const adapter = isPluginArray ? adapterOrPlugins[0] : adapterOrPlugins;

  const modelsForSchema: Partial<ModelsForSchema<TSchema, TAdapter>> = {};
  for (const key in schema) {
    modelsForSchema[key] = new adapter.Model(
      schema[key].$modelName ?? key,
      adapter,
      isPluginArray ? adapterOrPlugins : [],
    );
  }

  return {
    ...(modelsForSchema as ModelsForSchema<TSchema, TAdapter>),
    $adapter: adapter,
    $schema: schema,

    $plugin(plugin) {
      for (const key in this.$schema) {
        this[key].$plugins.push(plugin);
      }
    },
  } satisfies Kineo<TSchema, TAdapter>;
}
