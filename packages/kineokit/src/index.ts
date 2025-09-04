export interface Config {
  schemaFile: string;
  schemaExport: string;
  clientFile: string;
  clientExport: string;
  migrationsDir: string;
}

export function defineConfig(config: Config): Config {
  return config;
}
