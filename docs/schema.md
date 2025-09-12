---
sidebar_position: 3
---

# Schema

A schema represents the shape of your data. It is vital for type inference and consistency. To define a schema, you can use the `kineo/schema` package.

```ts
import {
  defineSchema,
  model,
  field,
  relation,
  type InferSchema,
} from "kineo/schema";

export const schema = defineSchema({
  // ...
});

export type Schema = InferSchema<typeof schema>;
```

This package comes with helpers (imported above) for creating a schema. `defineSchema` and `model` just add type definitions, while the rest actually define the schema.

Schemas are based on models. Models are objects containing fields and relational fields.

```ts
defineSchema({
  users: model({
    name: field.string().id(),
    password: field.string().required(),
    posts: relation.to("posts").outgoing("HAS_POSTS"),
  }),

  posts: model({
    // ...
  }),
});
```

A field can be of the following types:

- `any()`: Anything.
- `bool()`: A boolean.
- `date()`: A date.
- `duration()`: A time duration.
- `float()`: A floating point number.
- `integer()`: A non-floating point number
- `localDatetime()`: A local date time.
- `localTime()`: A local time.
- `map()`: A map.
- `nothing()`: Absence of value, or `null`.
- `path()`: Graph database specific. A path.
- `point()`: A point in space.
- `string()`: A string.
- `zonedDatetime()`: A zoned date time.
- `zonedTime()`: A zoned time.

A few modifiers can be applied and chained to fields:

- `id()`: Makes the field the primary key or unique ID.
- `default(any)`: Sets a default value.
- `required()`: Makes the field required.
- `optional()`: Makes the field not required.
- `array()`: Makes the field an array or list.

A relation can be created using the `to` function on the `relation` object (`relation.to("name")`). A few modifiers can be applied to relations:

- `label(string)`: Sets the label of the relation. This is more useful for graph databases.
- `outgoing(string)`: Marks the relationship as outgoing and sets the label. This is more useful for graph databases.
- `incoming(string)`: Marks the relationship as incoming and sets the label. This is more useful for graph databases.
- `both(string)`: Marks the relationship as going both ways and sets the label. This is more useful for graph databases.
- `required()`: Makes the relationship field as required.
- `default(any)`: Sets a default value.
- `array()`: Makes the relationship an array or list.
- `meta(object)`: Sets metadata for the relationship. Graph database specific.

## `InferSchema` type

The `InferSchema` type simply grabs type definitions from a schema you defined. As every field and relationship definition is a class, you can't just grab types from it — that's why this utility type exists: it extracts the types from the schema. You can use it with `InferSchema<SchemaType>`, like `InferSchema<typeof schema>`.
