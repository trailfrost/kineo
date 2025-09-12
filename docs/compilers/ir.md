---
sidebar_position: 1
---

# Intermediate representation

The IR or intermediate representation is a lower level representation of the options passed into a model. It makes it easier to compile into your database's query language. The IR types and parsers can be imported from `kineo/ir`.

## IR nodes

An IR contains a `statements` field with a list of nodes.

**Utility nodes:**

- `where` (`IRWhereNode`):
  - `AND: IRWhereNode[]`: A list of `where`s that must all be true.
  - `OR: IRWhereNode[]`: A list of `where`s that only one needs to be true.
  - `NOT: IRWhereNode`: A single `where` that must be false.
  - `conditions: IRWhereClause[]`: A list of `IRWhereClause`s to check.
- `IRWhereClause`:
  - `field: string`: A string representing the field you're comparing against.
  - `operator: "EQUALS" | "IN" | "NOT_IN" | "LT" | "LTE" | "GT" | "GTE" | "CONTAINS" | "STARTS_WITH" | "ENDS_WITH" | "NOT"`: A string representing the operator of the comparison node.
  - `value: unknown`: The value to compare. Can be anything.

**General query nodes:**

- `"MATCH"` (`IRMatch`): Equivalent to a `findOne`/`findMany` query.
  - `where: IRWhereNode`: Conditions to match.
  - `orderBy: Record<string, "asc" | "desc">`: Ordering options.
- `"CREATE"` (`IRCreate`): Equivalent to a `createOne`/`createMany` query.
  - `data: Record<string, unknown>`: An object containing the data to insert.
- `"MERGE"` (`IRMerge`): Equivalent to a `upsertOne`/`upsertMany` query.
  - `where: IRWhereNode`: Where to update or insert.
  - `create: Record<string, unknown>`: What to insert.
  - `update: Record<string, unknown>`: What to update.
- `"DELETE"` (`IRDelete`): Equivalent to a `deleteOne`/`deleteMany` query.
  - `where: IRWhereNode`: Where to delete.

**Graph database specific query nodes:**

- `"CONNECT" | "DISCONNECT"` (`IRConnect`): Equivalent to a `connect`/`disconnect` query.
  - `from`: Where to connect from.
    - `label: string`: The label of what to connect from.
    - `alias: string`: The alias of what to connect from.
    - `match: Record<string, unknown>`: What to match to connect from.
  - `to`: Where to connect from.
    - `label: string`: The label of what to connect to.
    - `alias: string`: The alias of what to connect to.
    - `match: Record<string, unknown>`: What to match to connect to.
  - `relation`: The identifier of the relation.
- `"RELATION_QUERY"`: Equivalent to a `getRelations` query.
  - `from`: Where to connect from.
    - `label: string`: The label of what to get the relationship properties.
    - `alias: string`: The alias of what to get the relationship properties.
    - `match: Record<string, unknown>`: What to match to get the relationship properties.
  - `relation`: The relation to get properties from.
- `"GET_NODE_LABELS"`: Equivalent to a `getNodeLabels` query.
- `"GET_RELATIONSHIP_TYPES"`: Equivalent to a `getRelationshipTypes` query.
- `"GET_NODE_PROPERTIES"`: Equivalent to a `getNodeProperties` query.
- `"GET_RELATIONSHIP_PROPERTIES"`: Equivalent to a `getRelationshipProperties` query.
  - `relationType: string`: The type of the relation to get.
