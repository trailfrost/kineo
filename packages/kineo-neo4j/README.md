# Kineo Neo4j adapter

Adapter for Neo4j for Kineo, allowing you to use Kineo as an OGM for Neo4j.

## Usage

```ts
Kineo(
  Neo4jAdapter({
    // URL to your database
    url: "bolt://localhost:7687/",
    auth: {
      // Authentication info
      username: "username",
      password: "password",
    },
  }),
  schema,
);
```
