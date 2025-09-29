import { IR } from "./ir.js";
import type { Model } from "./model.js";
import type { ModelDef, Schema } from "./schema.js";

// Either a Promise or not
type OptPromise<T> = T | Promise<T>;

/**
 * A result from a compiler.
 */
export interface CompileResult {
  command: string;
  params: Record<string, any>;
}

/**
 * Context passed into compilers.
 */
export interface CompilerContext {
  ir: IR;
  preset: any;
}

/**
 * A compiler.
 */
export type Compiler = (ctx: CompilerContext) => CompileResult;

/**
 * The result of a query.
 */
export type QueryResult = Map<string | number, Record<string, any>>;

/**
 * An adapter. Contains functions necessary to interact with the database of choice.
 * @param TModel The model class extension this adapter uses.
 */
export interface Adapter<TModel extends Model<any, any>> {
  /**
   * The name of the adapter.
   */
  name: string;
  /**
   * What extension of the model class you're using. This can be just the default model, `GraphModel` or a custom class.
   */
  Model: {
    new (schema: Schema, node: ModelDef, adapter: Adapter<TModel>): TModel;
  };

  // Runtime related functions
  /**
   * Compiles an intermediate representation into a query language.
   */
  compile(ctx: CompilerContext): OptPromise<CompileResult>;
  /**
   * Runs a compilation result against the database.
   * @param result The compile result.
   */
  exec(result: CompileResult): OptPromise<QueryResult>;
  /**
   * Closes the adapter.
   */
  close(): OptPromise<void>;

  // KineoKit related functions
  /**
   * Push a schema to the database. You don't need to warn the user, Kineo does that for you.
   * @param schema The schema to push.
   */
  push?(schema: Schema): OptPromise<void>;
  /**
   * Gets a schema from the database.
   */
  pull?(): OptPromise<Schema>;
  /**
   * Generates migrations.
   */
  generate?(): OptPromise<string[]>;
  /**
   * Gets a status for a migration.
   * @param migration The migration to get the status for.
   * @param hash The hash of the migration.
   */
  status?(migration: string, hash: string): OptPromise<"pending" | "completed">;
  /**
   * Deploys a migration.
   * @param migration The migration to deploy.
   * @param hash The hash of the migration.
   */
  deploy?(migration: string, hash: string): OptPromise<void>;
  /**
   * Rolls back a migration.
   * @param migration The migration to roll back.
   * @param hash The hash of the migration.
   */
  rollback?(migration: string, hash: string): OptPromise<void>;
}
