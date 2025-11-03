import path from "node:path";
import process from "node:process";
import { existsSync, promises as fs } from "node:fs";

import { color, Command, i, log, prompt } from "convoker";
import { createJiti } from "jiti";

import type { Kineo } from "@/client";
import type { Schema } from "@/schema";
import * as kit from ".";
import { KineoKitError } from "@/error";

const CONFIG_FILES = [
  "kineo.config.ts",
  "kineo.config.js",
  "kineo.config.mts",
  "kineo.config.mjs",
  "kineo.config.cts",
  "kineo.config.cjs",
];
const CWD = process.cwd();
const jiti = createJiti(CWD);

let config: kit.ParsedConfig | undefined;

const program = new Command("kineo")
  .version("0.6.0")
  .description("Manages migrations and schema.")
  .input({
    clientExport: i.option("string", "--client-export", "-x").optional(),
    clientFile: i.option("string", "--client-file", "-c").optional(),
    schemaExport: i.option("string", "--schema-export", "-e").optional(),
    schemaFile: i.option("string", "--schema-file", "-s").optional(),
    migrations: i.option("string", "--migrations-dir", "-m").optional(),
  })
  .use(
    async (
      { clientExport, clientFile, schemaExport, schemaFile, migrations },
      next
    ) => {
      await log.setup();
      if (!config) {
        for (const file of CONFIG_FILES) {
          try {
            const module = (await jiti.import(file, {
              default: true,
            })) as kit.KineoConfig;
            config = await kit.parseConfig(jiti, module);
            break;
          } catch (e) {
            await log.error(
              `an error occurred trying to import './${file}'${
                typeof e === "object" && e !== null && "message" in e
                  ? `: ${e.message}`
                  : ""
              }. trying to import next file.`
            );
            continue;
          }
        }

        if (!config) {
          await log.warn(
            "could not import a configuration file. make sure to create one using `kineo init` or pass options to this command. see `kineo --help` for more details."
          );
          config = {} as any;
        }
      }

      if (schemaFile) {
        config!.schemaMod = {
          file: schemaFile,
          export: schemaExport ?? "schema",
        };

        const mod = (await jiti.import(config!.schemaMod.file)) as Record<
          string,
          Schema
        >;
        config!.schema = mod[config!.schemaMod.export];
      } else {
        await log.warn(
          "config missing schemaFile. either pass `--schema-file` to this command or create a configuration file using `kineo init`."
        );
      }

      if (clientFile) {
        config!.clientMod = {
          file: clientFile,
          export: clientExport ?? "client",
        };

        const mod = (await jiti.import(config!.clientMod.file)) as Record<
          string,
          Kineo<any, any>
        >;
        config!.client = mod[config!.clientMod.export];
      } else {
        await log.warn(
          "config missing clientFile. either pass `--client-file` to this command or create a configuration file using `kineo init`."
        );
      }

      if (migrations) {
        config!.migrations = migrations;
      } else {
        await log.warn(
          "config missing migrations directory. either pass `--migrations-dir` to this command or create a configuration file using `kineo init`."
        );
      }

      await next();
    }
  );

program
  .subCommand("init", (c) =>
    c
      .description("Creates configuration for Kineo.")
      .input({
        clientExport: i.option("string", "--client-export", "-x").optional(),
        clientFile: i.option("string", "--client-file", "-c").optional(),
        schemaExport: i.option("string", "--schema-export", "-e").optional(),
        schemaFile: i.option("string", "--schema-file", "-s").optional(),
        migrations: i.option("string", "--migrations-dir", "-m").optional(),
        style: i.option("string", "--style", "-t").optional(),
      })
      .action(
        async ({
          clientExport,
          clientFile,
          schemaExport,
          schemaFile,
          migrations,
          style,
        }) => {
          if (!clientFile) {
            clientFile = await prompt.text({
              message: "Where is your client located?",
              default: "src/db/index.ts",
              placeholder: "src/db/index.ts",
            });
          }

          if (!clientExport) {
            clientExport = await prompt.text({
              message: "What is the name of the export of your client?",
              default: "client",
              placeholder: "client",
            });
          }

          if (!schemaFile) {
            schemaFile = await prompt.text({
              message: "Where is your schema located?",
              default: "src/db/index.ts",
              placeholder: "src/db/index.ts",
            });
          }

          if (!schemaExport) {
            schemaExport = await prompt.text({
              message: "What is the name of the export of your schema?",
              default: "schema",
              placeholder: "schema",
            });
          }

          if (!migrations) {
            migrations = await prompt.text({
              message: "Where do you want your migrations to be stored?",
              default: "migrations",
              placeholder: "migrations",
            });
          }

          if (!style) {
            style = await prompt.select({
              message:
                "How do you want your configuration file to be structured?",
              options: [
                {
                  label: "Dynamic imports",
                  value: "dynamic",
                },
                {
                  label: "Direct imports",
                  value: "direct",
                },
                {
                  label: "File paths",
                  value: "paths",
                },
                {
                  label: "CommonJS",
                  value: "commonjs",
                },
                {
                  label: "CommonJS file paths",
                  value: "commonjs-paths",
                },
              ],
            });
          }

          await log.info("\nGenerating configuration file.");

          let contents: string;
          let fileName = "kineo.config.ts";
          if (style === "direct") {
            contents = `import { defineConfig } from "kineo/kit";
import ${schemaExport === "default" ? "schema" : `{ ${schemaExport} as schema }`} from ${importPath(schemaFile)};
import ${clientExport === "default" ? "client" : `{ ${clientExport} as client }`} from ${importPath(clientFile)};

export default defineConfig({
  schema,
  client,
  migrations: ${importPath(migrations)},
});
`;
          } else if (style === "dynamic") {
            contents = `import { defineConfig } from "kineo/kit";

export default defineConfig({
  schema: import(${importPath(schemaFile)}).then((mod) => mod["${schemaExport}"]),
  client: import(${importPath(clientFile)}).then((mod) => mod["${clientExport}"]),
  migrations: ${importPath(migrations)},
});
`;
          } else if (style === "paths") {
            contents = `import { defineConfig } from "kineo/kit";

export default defineConfig({
  schema: { file: ${importPath(schemaFile)}, export: "${schemaExport}" },
  client: { file: ${importPath(clientFile)}, export: "${clientExport}" },
  migrations: ${importPath(migrations)},
});
`;
          } else if (style === "commonjs") {
            contents = `const { defineConfig } = require("kineo/kit");

module.exports = defineConfig({
  schema: require(${importPath(schemaFile)})["${schemaExport}"],
  client: require(${importPath(clientFile)})["${clientExport}"],
  migrations: ${importPath(migrations)}
});
`;
            fileName = "kineo.config.cts";
          } else {
            contents = `const { defineConfig } = require("kineo/kit");

module.exports = defineConfig({
  schema: { file: ${importPath(schemaFile)}, export: "${schemaExport}" },
  client: { file: ${importPath(clientFile)}, export: "${clientExport}" },
  migrations: ${importPath(migrations)},
});
`;
            fileName = "kineo.config.cts";
          }

          await log.trace("writing", contents, "to", fileName);

          await fs.writeFile(path.join(CWD, fileName), contents, "utf-8");

          await log.info(
            "Configuration file generated! You can now start using Kineo migrations."
          );
        }
      )
  )
  .subCommand("push", (c) =>
    c
      .description(
        "Pushes the current schema to the database, warning you for breaking changes."
      )
      .input({
        force: i.option("boolean", "-f", "--force").optional(),
      })
      .action(async ({ force }) => {
        try {
          await kit.push(config!.client.$adapter, config!.schema, force);
        } catch (e) {
          if (e instanceof KineoKitError) {
            const { data } = e as KineoKitError<kit.SchemaDiff>;
            if ((data?.breaking.length ?? 0) > 0) {
              await log.info(
                `Changes:\n${color.bold("- Breaking:")}\n${data?.breaking.map((entry) => `  ${entry}`).join("\n")}
${color.bold("- Not Breaking:")}\n${data?.nonBreaking.map((entry) => `  ${entry}`)}`
              );
              const confirmed = await prompt.confirm({
                message:
                  "A breaking change was detected. PUSHING THE SCHEMA WILL CAUSE DATA LOSS. Proceed anyways?",
              });

              if (confirmed)
                await kit.push(config!.client.$adapter, config!.schema, true);
            }
          }

          throw e;
        }
      })
  )
  .subCommand("pull", (c) =>
    c
      .description(
        "Pulls the current schema from the database. This only works for file path style imports in the configuration."
      )
      .input({
        force: i.option("boolean", "-f", "--force").optional(),
      })
      .action(async ({ force }) => {
        // TODO
        await log.fatal("Not implemented", force);
      })
  )
  .subCommand(["generate", "migrate"], (c) =>
    c
      .description(
        "Generates migrations based on the current database state and the current schema."
      )
      .action(async () => {
        const adapter = config!.client.$adapter;
        const migrations = await kit.generate(
          adapter,
          await kit.pull(adapter),
          config!.schema
        );
        await Promise.all(
          migrations.map((migration) =>
            fs.writeFile(
              path.join(
                CWD,
                config!.migrations,
                `${Date.now()}.${adapter.fileExt}`
              ),
              migration,
              "utf-8"
            )
          )
        );
      })
  )
  .subCommand("status", (c) =>
    c.description("Gets status for existing migrations.").action(async () => {
      const entries = await fs.readdir(path.join(CWD, config!.migrations));

      const statuses = await Promise.all(
        entries.map(async (entry) => {
          const migration = await fs.readFile(
            path.join(CWD, config!.migrations, entry),
            "utf-8"
          );
          return {
            entry,
            status: await kit.status(config!.client.$adapter, migration),
          };
        })
      );

      for (const status of statuses) {
        await log.info(`${status.entry}: ${status.status}`);
      }
    })
  )
  .subCommand("create", (c) =>
    c
      .description("Creates a new migration file.")
      .input({
        name: i.option("string", "-n", "--name"),
      })
      .action(async ({ name }) => {
        const filePath = path.join(
          CWD,
          config!.migrations,
          `${name ?? Date.now()}.${config!.client.$adapter.fileExt}`
        );
        await log.trace("creating migration", filePath);

        if (!existsSync(filePath)) {
          const confirmed = await prompt.confirm({
            message: `The file ${filePath} already exists. Would you like to override it?`,
          });
          if (!confirmed) return;
        }

        await fs.writeFile(filePath, "", "utf-8");
      })
  )
  .subCommand("deploy", (c) =>
    c.description("Deploys existing migrations.").action(async () => {
      const entries = await fs.readdir(path.join(CWD, config!.migrations));

      await Promise.all(
        entries.map(async (entry) => {
          const migration = await fs.readFile(
            path.join(CWD, config!.migrations, entry),
            "utf-8"
          );
          const status = await kit.status(config!.client.$adapter, migration);
          if (status === "completed") return;

          await kit.deploy(config!.client.$adapter, migration);
        })
      );
    })
  )
  .subCommand("rollback", (c) =>
    c
      .description("Rolls back a certain number of migrations.")
      .input({
        n: i.positional("number"),
      })
      .action(async ({ n }) => {
        const entries = await fs.readdir(path.join(CWD, config!.migrations));
        const sortedEntries = await Promise.all(
          entries.map(async (entry) => {
            const fullPath = path.join(CWD, config!.migrations, entry);
            const stat = await fs.stat(fullPath);
            return { entry: fullPath, mtime: stat.mtime };
          })
        ).then((entries) =>
          entries
            .sort((a, b) => b.mtime.getTime() - a.mtime.getTime())
            .map(({ entry }) => entry)
        );

        await Promise.all(
          sortedEntries.slice(0, n).map(async (entry) => {
            const migration = await fs.readFile(entry, "utf-8");
            await kit.rollback(config!.client.$adapter, migration);
          })
        );
      })
  );

void program.run();

function importPath(file: string) {
  return `"${file.startsWith(".") ? file : `./${file}`}"`;
}
