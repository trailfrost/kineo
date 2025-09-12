# Neo4j adapter

The Neo4j adapter is an adapter that uses `neo4j-driver@^5.0.0` to talk to Neo4j using Kineo.

## Usage

You pass the Neo4j adapter as the first parameter to the `Kineo` factory function from `kineo`.

```ts
import Kineo from "kineo";
import Neo4jAdapter from "kineo/adapters/neo4j";

Kineo(Neo4jAdapter(...), schema);
```

It can take in two types of options.

1. an **object** with a **single `driver` key** with a Neo4j driver:

   ```ts
   import neo4j from "neo4j-driver";
   import Neo4jAdapter from "kineo/adapters/neo4j";

   const driver = neo4j.driver(
     "bolt://localhost:7687",
     neo4j.auth.basic(/* ... */)
   );

   Neo4jAdapter({ driver });
   ```

2. an object with **options for creating a driver**:

   ```ts
   import Neo4jAdapter from "kineo/adapters/neo4j";

   Neo4jAdapter({
     url: "bolt://localhost:7687",
     auth: {
       username: "neo4j",
       password: "password",
     },
   });
   ```

   The `auth` object can be one of the following:
   1. **Basic authentication**, by omitting `type` or setting `type: "basic"`. Username/password/realm.

   ```ts
   auth: {
    type?: "basic",
    username: string,
    password: string,
    realm?: string,
   }
   ```

   2. **Bearer authentication**, by setting `type: "bearer"`. Bearer token authentication.

   ```ts
   auth: {
    type: "bearer",
    token: string,
   }
   ```

   3. **Kerberos authentication**, by setting `type: "kerberos"`. Ticket authentication.

   ```ts
   auth: {
    type: "kerberos",
    ticket: string,
   }
   ```

   4. **Custom authentication**, by setting `type: "custom"`. Custom configuration.

   ```ts
   auth: {
    type: "custom",
    principal: string,
    credentials: string,
    realm: string,
    scheme: string,
    parameters: Record<string, unknown>,
   }
   ```
