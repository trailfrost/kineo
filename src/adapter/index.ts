import type { IR } from "@/ir";
import type { Model } from "@/model";
import type { Schema } from "@/schema";

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
 * A compiler.
 */
export type Compiler<T = any> = (ir: IR, preset?: T) => CompileResult;

/**
 * Result of executing a query.
 */
export interface ExecResult<T = any> {
  entries: Record<string, any>[];
  entryCount: number;
  edges?: {
    type: string;
    direction: "incoming" | "outgoing";
    props?: any;
    from?: string | number;
    to?: string | number;
  }[];
  edgeCount?: number;

  summary?: T;
  raw?: unknown;
}

/**
 * An adapter. Contains functions necessary to interact with the database of choice.
 */
export interface Adapter<
  TModelCtor extends {
    new (name: string, adapter: Adapter<any, any>): Model<any, any>;
  },
  Summary = any,
> {
  /**
   * What extension of the model class you're using. This can be just the default model or `GraphModel`. Right now, this can't be a custom class.
   */
  Model: TModelCtor;
  /**
   * What the file extension for migrations should be.
   */
  fileExt?: string;

  // Runtime related functions
  /**
   * Compiles an intermediate representation into a query language.
   */
  compile(ir: IR): OptPromise<CompileResult>;
  /**
   * Runs a compilation result against the database.
   * @param result The compile result.
   */
  exec(result: CompileResult): OptPromise<ExecResult<Summary>>;
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
  generate?(prev: Schema, cur: Schema): OptPromise<string[]>;
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
