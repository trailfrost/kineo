import { KineoClient } from "@/client.js";
import { Schema } from "@/schema.js";

export interface FileExport {
  file: string;
  export: string;
}

export interface KineoConfig {
  schema: string | FileExport | (() => Schema | Promise<Schema>);
  client:
    | string
    | FileExport
    | (() => KineoClient<any, any> | Promise<KineoClient<any, any>>);
  migrations: string;
}

export function defineConfig(config: KineoConfig) {
  return config;
}
