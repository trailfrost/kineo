import type { Kineo } from "@/client";
import type { Schema } from "@/schema";

export interface FileExport {
  file: string;
  export: string;
}

export type ReferenceFn<T> = () => Promise<T> | T;
export type Reference<T> =
  | string
  | FileExport
  | T
  | Promise<T>
  | ReferenceFn<T>;

export interface KineoConfig {
  schema: Reference<Schema>;
  client: Reference<Kineo<any, any>>;
  migrations: string;
}

export function defineConfig(config: KineoConfig) {
  return config;
}
