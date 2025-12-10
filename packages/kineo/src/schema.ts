/**
 * A schema. Contains model definitions.
 */
export interface Schema {
  [model: string]: ModelDef;
}

/**
 * A model definition.
 * Note: you will have to check if the field is an instance of FieldDef or RelationDef manually, as due to TypeScript index signature constraints, each key can be a string or undefined.
 */
export interface ModelDef {
  $modelName?: string;
  [key: string]:
    | FieldDef<any, any, any, any>
    | RelationDef<any, any, any, any>
    | string
    | undefined;
}

/**
 * Finds a model by its internal name.
 */
export type FindModelByName<TSchema extends Schema, TName extends string> = {
  [K in keyof TSchema]: TSchema[K]["$modelName"] extends string // If user specified a name -> match it
    ? TSchema[K]["$modelName"] extends TName
      ? TSchema[K]
      : never
    : // Otherwise fall back to schema key
      K extends TName
      ? TSchema[K]
      : never;
}[keyof TSchema];

/**
 * Infers a type from a field definition.
 */
export type InferField<TField extends FieldDef<any, any, any, any>> =
  TField extends FieldDef<
    infer K,
    infer TDefault,
    infer TRequired,
    infer TArray
  >
    ? // base value (array or single)
      (TArray extends true ? TypeOf<K>[] : TypeOf<K>) extends infer Base
      ? // if required or has a default => definitely Base, otherwise allow undefined
        TRequired extends true
        ? Base
        : TDefault extends undefined
          ? Base | undefined
          : Base
      : never
    : never;

/**
 * Infers a type from a relationship definition.
 */
export type InferRelationship<
  TRelation extends RelationDef<any, any, any, any>,
  TSchema extends Schema,
> =
  TRelation extends RelationDef<
    infer To,
    infer TDefault,
    infer TRequired,
    infer TArray
  >
    ? FindModelByName<TSchema, To> extends infer TargetModel
      ? TargetModel extends ModelDef
        ? (
            TArray extends true
              ? InferModelDef<TargetModel, TSchema>[]
              : InferModelDef<TargetModel, TSchema>
          ) extends infer Base
          ? TRequired extends true
            ? Base
            : TDefault extends undefined
              ? Base | undefined
              : Base
          : never
        : unknown
      : unknown
    : never;

/**
 * Infer a single model's properties.
 */
export type InferModelDef<
  TDef extends ModelDef,
  TSchema extends Schema = Schema,
> = {
  [P in keyof TDef]: TDef[P] extends FieldDef<any, any, any, any>
    ? InferField<TDef[P]>
    : TDef[P] extends RelationDef<any, any, any, any>
      ? InferRelationship<TDef[P], TSchema>
      : never;
};

/**
 * Infers a whole schema.
 */
export type InferSchema<TSchema extends Schema> = {
  [M in keyof TSchema]: InferModelDef<TSchema[M], TSchema>;
};

/**
 * All supported field types.
 */
export type Kind =
  | "string"
  | "char"
  | "int"
  | "float"
  | "date"
  | "time"
  | "datetime"
  | "timestamp"
  | "bool"
  | "blob";

/**
 * Converts a `Kind` string into a TypeScript type.
 */
export type TypeOf<TKind extends Kind> = TKind extends "string" | "char"
  ? string
  : TKind extends "int" | "float"
    ? number
    : TKind extends "date" | "time" | "datetime" | "timestamp"
      ? Date
      : TKind extends "bool"
        ? boolean
        : TKind extends "blob"
          ? Blob
          : never;

/**
 * Relationship direction (for graph databases).
 */
export type Direction = "incoming" | "outgoing" | "both";

/**
 * A field definition.
 */
export class FieldDef<
  TKind extends Kind,
  TDefault = undefined,
  TRequired extends boolean = false,
  TArray extends boolean = false,
> {
  /**
   * The type of the field.
   */
  kind: TKind;
  /**
   * The name of the field, in case you want to override the default name (which is the key in the model).
   */
  rowName: string | undefined;
  /**
   * The default value of the field.
   */
  defaultValue: TDefault = undefined as TDefault;
  /**
   * If this field is required.
   */
  isRequired: TRequired = false as TRequired;
  /**
   * If this field is the identifier (or primary key).
   */
  isId = false;
  /**
   * If this field is an array.
   */
  isArray: TArray = false as TArray;

  /**
   * Creates a new field definition.
   * @param kind The type of the field.
   * @param name _optional_ The name of the field.
   */
  constructor(kind: TKind, name?: string) {
    this.kind = kind;
    this.rowName = name;
  }

  /**
   * Changes the type of the field.
   * @param kind The new type.
   * @returns `this`.
   */
  type<T extends Kind>(kind: T) {
    this.kind = kind as any as TKind;
    return this as any as FieldDef<T, TDefault, TRequired, TArray>;
  }

  /**
   * Changes the name of the field.
   * @param rowName The new name.
   * @returns `this`.
   */
  name(rowName: string) {
    this.rowName = rowName;
    return this;
  }

  /**
   * Changes the default value of the field.
   * @param kind The new default value.
   * @returns `this`.
   */
  default<T>(value: T) {
    this.defaultValue = value as any as TDefault;
    return this as any as FieldDef<TKind, T, TRequired, TArray>;
  }

  /**
   * Makes this field required.
   * @returns `this`.
   */
  required() {
    this.isRequired = true as TRequired;
    return this as FieldDef<TKind, TDefault, true, TArray>;
  }

  /**
   * Makes this field optional.
   * @returns `this`.
   */
  optional() {
    this.isRequired = false as TRequired;
    return this as FieldDef<TKind, TDefault, false, TArray>;
  }

  /**
   * Makes this field the identifier (or primary key).
   * @returns `this`.
   */
  id() {
    this.isId = true;
    return this as FieldDef<TKind, TDefault, true, TArray>;
  }

  /**
   * Makes this field an array.
   * @returns `this`.
   */
  array() {
    this.isArray = true as TArray;
    return this as FieldDef<TKind, TDefault, TRequired, true>;
  }

  /**
   * Makes this field not an array.
   * @returns `this`.
   */
  single() {
    this.isArray = false as TArray;
    return this as FieldDef<TKind, TDefault, TRequired, false>;
  }
}

/**
 * A relationship definition.
 */
export class RelationDef<
  To extends string,
  TDefault = undefined,
  TRequired extends boolean = false,
  TArray extends boolean = false,
> {
  /**
   * Where this relationship is pointing to.
   */
  pointTo: To;
  /**
   * The name of the relationship, in case you want to override the default name (which is the key in the model).
   */
  relName: string | undefined;
  /**
   * The label of the relationship, used in graph databases.
   */
  relLabel: string | undefined;
  /**
   * The direction of the relationship, used in graph databases.
   */
  relDirection: Direction = "both";
  /**
   * The default value of the relationship.
   */
  defaultValue: TDefault = undefined as TDefault;
  /**
   * If this relationship is required or not.
   */
  isRequired: TRequired = false as TRequired;
  /**
   * If this relationship is an array.
   */
  isArray: TArray = false as TArray;

  /**
   * Creates a new relationship definition.
   * @param to Where this relationship is pointing to.
   * @param name _optional_ The name of the relationship.
   */
  constructor(to: To, name?: string) {
    this.relName = name;
    this.pointTo = to;
  }

  /**
   * Changes where this relationship is pointing to.
   * @param pointTo Where this relationship is pointing to.
   * @returns `this`.
   */
  to<T extends string>(pointTo: T) {
    this.pointTo = pointTo as any as To;
    return this as any as RelationDef<T, TDefault, TRequired, TArray>;
  }

  /**
   * Changes the name of the relationship.
   * @param relName The new name.
   * @returns `this`.
   */
  name(relName: string) {
    this.relName = relName;
    return this;
  }

  /**
   * Changes the label of the relationship.
   * @param relLabel The new label.
   * @returns `this`.
   */
  label(relLabel: string) {
    this.relLabel = relLabel;
    return this;
  }

  /**
   * Changes the direction of the relationship.
   * @param relDirection The new direction.
   * @returns `this`.
   */
  direction(relDirection: Direction) {
    this.relDirection = relDirection;
    return this;
  }

  /**
   * Makes relationship outgoing and optionally sets a label.
   * @param label _optional_ The new label.
   * @returns `this`.
   */
  outgoing(label?: string) {
    this.relDirection = "outgoing";
    if (label) this.relLabel = label;
    return this;
  }

  /**
   * Makes the relationship incoming and optionally sets a label.
   * @param label _optional_ The new label.
   * @returns `this`.
   */
  incoming(label?: string) {
    this.relDirection = "incoming";
    if (label) this.relLabel = label;
    return this;
  }

  /**
   * Makes the relationship go both directions and optionally sets a label.
   * @param label _optional_ The new label.
   * @returns `this`.
   */
  both(label?: string) {
    this.relDirection = "both";
    if (label) this.relLabel = label;
    return this;
  }

  /**
   * Changes the default value.
   * @param defaultValue The new default value.
   * @returns `this`.
   */
  default<T>(defaultValue: T) {
    this.defaultValue = defaultValue as any as TDefault;
    return this as any as RelationDef<To, T, TRequired, TArray>;
  }

  /**
   * Makes this relationship required.
   * @returns `this`.
   */
  required() {
    this.isRequired = true as TRequired;
    return this as RelationDef<To, TDefault, true, TArray>;
  }

  /**
   * Makes this relationship optional.
   * @returns `this`.
   */
  optional() {
    this.isRequired = false as TRequired;
    return this as RelationDef<To, TDefault, false, TArray>;
  }

  /**
   * Makes this relationship an array.
   * @returns `this`.
   */
  array() {
    this.isArray = true as TArray;
    return this as RelationDef<To, TDefault, TRequired, true>;
  }

  /**
   * Makes this relationship a single element.
   * @returns `this`.
   */
  single() {
    this.isArray = false as TArray;
    return this as RelationDef<To, TDefault, TRequired, false>;
  }
}

/**
 * Utilities for creating field definitions.
 */
export const field = {
  /**
   * Creates a new string field definition.
   * @param name _optional_ The name of the field.
   * @returns A string field definition.
   */
  string: (name?: string) => new FieldDef("string", name),
  /**
   * Creates a new char field definition.
   * @param name _optional_ The name of the field.
   * @returns A char field definition.
   */
  char: (name?: string) => new FieldDef("char", name),
  /**
   * Creates a new integer field definition.
   * @param name _optional_ The name of the field.
   * @returns A int field definition.
   */
  int: (name?: string) => new FieldDef("int", name),
  /**
   * Creates a new floating point field definition.
   * @param name _optional_ The name of the field.
   * @returns A floating point field definition.
   */
  float: (name?: string) => new FieldDef("float", name),
  /**
   * Creates a new date field definition.
   * @param name _optional_ The name of the field.
   * @returns A date field definition.
   */
  date: (name?: string) => new FieldDef("date", name),
  /**
   * Creates a new time field definition.
   * @param name _optional_ The name of the field.
   * @returns A time field definition.
   */
  time: (name?: string) => new FieldDef("time", name),
  /**
   * Creates a new datetime field definition.
   * @param name _optional_ The name of the field.
   * @returns A datetime field definition.
   */
  datetime: (name?: string) => new FieldDef("datetime", name),
  /**
   * Creates a new timestamp field definition.
   * @param name _optional_ The name of the field.
   * @returns A timestamp field definition.
   */
  timestamp: (name?: string) => new FieldDef("timestamp", name),
  /**
   * Creates a new boolean field definition.
   * @param name _optional_ The name of the field.
   * @returns A boolean field definition.
   */
  bool: (name?: string) => new FieldDef("bool", name),
  /**
   * Creates a new boolean field definition.
   * @param name _optional_ The name of the field.
   * @returns A boolean field definition.
   */
  boolean: (name?: string) => new FieldDef("bool", name),
};

/**
 * Utilities for creating relationship definitions.
 */
export const relation = {
  /**
   * Creates a new relationship definition.
   * @param to Where this relationship is pointing to.
   * @param name _optional_ The name of the relationship.
   * @returns A new relationship definition.
   */
  to: (to: string, name?: string) => new RelationDef(to, name),
};

/**
 * Creates a new model.
 * @param name The model name in the database. This does not change the model name in your schema.
 * @param model The model definition.
 * @returns The created model;
 */
export function model<TName extends string, TModel extends ModelDef>(
  name: TName,
  model: TModel,
): TModel & { $modelName: TName };

/**
 * Creates a new model.
 * @param model The model definition.
 * @returns The created model.
 */
export function model<TModel extends ModelDef>(
  model: TModel,
): TModel & { $modelName: undefined };

export function model<TName extends string, TModel extends ModelDef>(
  nameOrModel: TName | TModel,
  def?: TModel,
) {
  if (typeof nameOrModel === "string") {
    return Object.assign(def!, { $modelName: nameOrModel });
  }

  return nameOrModel;
}

/**
 * Adds schema type definitions to an object.
 * @param schema The object.
 * @returns The same object.
 */
export function defineSchema<TSchema extends Schema>(schema: TSchema): TSchema {
  return schema;
}
