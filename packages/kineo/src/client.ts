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
   * Adds and initializes a plugin.
   * @param plugin The plugin to add and init.
   */
  $plugin(plugin: Plugin): void;
  /**
   * Adds a plugin without initializing it.
   */
  $addPlugin(plugin: Plugin): void;
  /**
   * Initializes an already added plugin.
   * @param plugin The plugin to initialize.
   */
  $initPlugin(plugin: Plugin): void;
  /**
   * Extends a schema at a runtime level. This does not change type definitions, meaning the merged properties or models won't be accessible to users without type assertions.
   */
  $extendSchema(...extensions: Schema[]): void;
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
      isPluginArray ? adapterOrPlugins : []
    );
  }

  const client: Kineo<TSchema, TAdapter> = {
    ...(modelsForSchema as ModelsForSchema<TSchema, TAdapter>),
    $adapter: adapter,
    $schema: schema,

    $extendSchema(...extensions) {
      deepMerge(this.$schema, ...extensions);
      for (const key in this.$schema) {
        if (!this[key])
          this[key] = new adapter.Model(
            this.$schema[key].$modelName ?? key,
            adapter,
            isPluginArray ? adapterOrPlugins : []
          );
      }
    },

    $plugin(plugin) {
      this.$addPlugin(plugin);
      this.$initPlugin(plugin);
    },

    $addPlugin(plugin) {
      for (const key in this.$schema) {
        this[key].$plugins.push(plugin);
      }
    },

    $initPlugin(plugin) {
      plugin.init?.(this);
    },
  };

  if (isPluginArray) {
    for (const plugin of adapterOrPlugins) {
      client.$initPlugin(plugin as Plugin);
    }
  }

  return client;
}

// --- deep merge (util only used here) --- //

type PlainObject = { [key: string]: any };

/**
 * Returns true if value is a mergeable object (created by {}, new Object or a clsas instance).
 */
function isMergeableObject(value: any): boolean {
  return value !== null && typeof value === "object" && !Array.isArray(value);
}

/**
 * Deeply merges source objects into target *in-place*.
 */
function deepMerge<T extends PlainObject>(target: T, ...sources: any[]) {
  const dangerousKeys = new Set(["__proto__", "constructor", "prototype"]);

  for (const source of sources) {
    if (source === null || source === undefined) continue;

    // NEW: allow any object (model definitions, classes, objects with prototypes)
    if (!isMergeableObject(source)) continue;

    for (const key of Object.keys(source)) {
      if (dangerousKeys.has(key)) continue;

      const srcVal = source[key];
      const tgtVal = target[key];

      // recursive merge for any non-array object
      if (isMergeableObject(srcVal) && isMergeableObject(tgtVal)) {
        deepMerge(tgtVal, srcVal);
        continue;
      }

      if (Array.isArray(srcVal)) {
        (target as any)[key] = srcVal.slice();
        continue;
      }

      if (isMergeableObject(srcVal) && !isMergeableObject(tgtVal)) {
        (target as any)[key] = {};
        deepMerge(target[key], srcVal);
        continue;
      }

      (target as any)[key] = srcVal;
    }
  }
}
