/**
 * All supported Kineo types.
 */
export type KineoType =
  | "ANY"
  | "BOOLEAN"
  | "DATE"
  | "DURATION"
  | "FLOAT"
  | "INTEGER"
  | "LOCAL DATETIME"
  | "LOCAL TIME"
  | "MAP"
  | "NOTHING"
  | "PATH"
  | "POINT"
  | "STRING"
  | "ZONED DATETIME"
  | "ZONED TIME";

/**
 * Relationship directions.
 */
export type Direction = "IN" | "OUT" | "BOTH";

/**
 * A Neo4j node.
 */
export type Node = Record<
  string,
  FieldDef<KineoType> | RelationshipDef<string>
>;

/**
 * Type for a schema.
 */
export type Schema = Record<string, Node>;

/**
 * Converts a Kineo type to a TypeScript type.
 */
type KineoToTs<T extends KineoType> = T extends "ANY"
  ? unknown
  : T extends
        | "ZONED DATETIME"
        | "ZONED TIME"
        | "LOCAL DATETIME"
        | "LOCAL TIME"
        | "DATE"
    ? Date
    : T extends "DURATION"
      ? Duration
      : T extends "BOOLEAN"
        ? boolean
        : T extends "FLOAT" | "INTEGER"
          ? number
          : T extends "MAP"
            ? Record<string, unknown>
            : T extends "NOTHING"
              ? null | undefined
              : T extends "PATH"
                ? Path
                : T extends "POINT"
                  ? Point
                  : T extends "STRING"
                    ? string
                    : never;

/**
 * Infers a value from a Kineo type.
 */
type InferValue<
  T extends KineoType,
  IsRequired extends boolean,
  IsArray extends boolean,
> = IsArray extends true
  ? Array<KineoToTs<T>>
  : IsRequired extends true
    ? KineoToTs<T>
    : KineoToTs<T> | undefined;

/**
 * Infers a relationship from a schema.
 */
type InferRelationship<
  T extends Schema,
  To extends string,
  IsRequired extends boolean,
  IsArray extends boolean,
> = IsArray extends true
  ? Array<InferNode<T[To], T>>
  : IsRequired extends true
    ? InferNode<T[To], T>
    : InferNode<T[To], T> | undefined;

/**
 * Is the field required?
 */
type IsRequiredField<T> =
  // eslint-disable-next-line
  T extends FieldDef<any, infer Req, any>
    ? Req
    : // eslint-disable-next-line
      T extends RelationshipDef<any, infer Req, any>
      ? Req
      : false;

/**
 * Infers a node from a schema.
 */
export type InferNode<TNode extends Node, TSchema extends Schema> =
  // Required fields
  {
    [K in keyof TNode as IsRequiredField<TNode[K]> extends true
      ? K
      : never]: TNode[K] extends FieldDef<infer Type, true, infer Array>
      ? InferValue<Type, true, Array>
      : TNode[K] extends RelationshipDef<infer To, true, infer Array>
        ? InferRelationship<TSchema, To, true, Array>
        : never;
  } & {
    // Optional fields
    [K in keyof TNode as IsRequiredField<TNode[K]> extends false
      ? K
      : never]?: TNode[K] extends FieldDef<infer Type, false, infer Array>
      ? InferValue<Type, false, Array>
      : TNode[K] extends RelationshipDef<infer To, false, infer Array>
        ? InferRelationship<TSchema, To, false, Array>
        : never;
  };

/**
 * Converts a schema to TypeScript types.
 */
export type InferSchema<T extends Schema> = {
  [key in keyof T]: InferNode<T[key], T>;
};

/**
 * Field definition.
 */
export class FieldDef<
  TType extends KineoType,
  IsRequired extends boolean = false,
  IsArray extends boolean = false,
  IsId extends boolean = false,
> {
  /**
   * Kineo type this field maps to.
   */
  fieldType: TType;
  /**
   * Default value.
   */
  defaultValue: unknown | null;
  /**
   * Is this the primary ID?
   */
  isPrimaryKey: boolean = false;
  /**
   * Is this field required?
   */
  isRequired: boolean = false;
  /**
   * Is this field unique?
   */
  isUnique: boolean = false;
  /**
   * Is this field an array?
   */
  isArray: boolean = false;

  /**
   * Creates a new field.
   * @param type The Kineo type of this field.
   */
  constructor(type: TType) {
    this.fieldType = type;
  }

  /**
   * Marks this field as the primary ID and as required.
   * @returns this
   */
  id() {
    this.isPrimaryKey = true;
    this.isRequired = true;
    return this as FieldDef<TType, true, IsArray, true>;
  }

  /**
   * Sets a default value.
   * @param value Default value.
   * @returns this
   */
  default(value: unknown) {
    this.defaultValue = value;
    this.isRequired = true;
    return this as FieldDef<TType, false, IsArray, IsId>;
  }

  /**
   * Sets this field as required.
   * @returns this
   */
  required() {
    this.isRequired = true;
    return this as FieldDef<TType, true, IsArray, IsId>;
  }

  /**
   * Sets this field as optional.
   * @returns this
   */
  optional() {
    this.isRequired = false;
    return this as FieldDef<TType, false, IsArray, IsId>;
  }

  /**
   * Sets this field as an array.
   * @returns this
   */
  array() {
    this.isArray = true;
    if (!this.defaultValue) this.defaultValue = [];
    return this as FieldDef<TType, IsRequired, true, IsId>;
  }
}

/**
 * Relationship definition.
 */
export class RelationshipDef<
  To extends string,
  IsRequired extends boolean = false,
  IsArray extends boolean = false,
  Metadata extends object | undefined = undefined,
> {
  /**
   * What is this relation referencing?
   */
  refTo: To;
  /**
   * What is the label of this relation?
   */
  refLabel: string = "UNNAMED";
  /**
   * In which direction is this relationship going? Incoming, outgoing or both?
   */
  refDirection: Direction = "OUT";
  /**
   * Is this relationship required?
   */
  isRequired: boolean = false;
  /**
   * Is this relationship an array?
   */
  isArray: boolean = false;
  /**
   * Default value.
   */
  defaultValue: unknown | null;
  /**
   * Metadata associated with the relationship.
   */
  metadata: Metadata = undefined as Metadata;

  /**
   * Creates a new relationship.
   * @param to What this relationship is referencing.
   */
  constructor(to: To) {
    this.refTo = to;
  }

  /**
   * Sets the label of this relationship.
   * @param name The label.
   * @returns this
   */
  label(name: string) {
    this.refLabel = name;
    return this;
  }

  /**
   * Sets the label and sets the direction to OUT.
   * @param label The label.
   * @returns this
   */
  outgoing(label: string) {
    this.refDirection = "OUT";
    this.refLabel = label;
    return this;
  }

  /**
   * Sets the label and sets the direction to IN.
   * @param label The label.
   * @returns this
   */
  incoming(label: string) {
    this.refDirection = "IN";
    this.refLabel = label;
    return this;
  }

  /**
   * Sets the label and sets the direction to BOTH.
   * @param label The label.
   * @returns this
   */
  both(label: string) {
    this.refDirection = "BOTH";
    this.refLabel = label;
    return this;
  }

  /**
   * Marks this relationship as required.
   * @returns this
   */
  required() {
    this.isRequired = true;
    return this as RelationshipDef<To, true, IsArray, Metadata>;
  }

  /**
   * Adds a default value.
   * @param value The default value.
   * @returns this
   */
  default(value: unknown) {
    this.defaultValue = value;
    return this as RelationshipDef<To, false, IsArray, Metadata>;
  }

  /**
   * Marks this relationship as a list/array.
   * @returns this
   */
  array() {
    this.isArray = true;
    return this as RelationshipDef<To, IsRequired, true, Metadata>;
  }

  /**
   * Adds metadata to this relationship.
   * @param metadata The metadata to add.
   * @returns this
   */
  meta<T extends object>(metadata: T) {
    this.metadata = metadata as unknown as Metadata;
    return this as unknown as RelationshipDef<To, IsRequired, IsArray, T>;
  }
}

/**
 * Utility for creating field definitions.
 */
export const field = {
  /**
   * Returns a field definition for any type.
   * @returns A new field definition.
   */
  any: () => new FieldDef("ANY"),
  /**
   * Returns a boolean field definition.
   * @returns A new field definition.
   */
  bool: () => new FieldDef("BOOLEAN"),
  /**
   * Returns a boolean field definition.
   * @returns A new field definition.
   */
  boolean: () => new FieldDef("BOOLEAN"),
  /**
   * Returns a date field definition.
   * @returns A new field definition.
   */
  date: () => new FieldDef("DATE"),
  /**
   * Returns a duration field definition.
   * @returns A new field definition.
   */
  duration: () => new FieldDef("DURATION"),
  /**
   * Returns a float field definition.
   * @returns A new field definition.
   */
  float: () => new FieldDef("FLOAT"),
  /**
   * Returns an integer field definition.
   * @returns A new field definition.
   */
  integer: () => new FieldDef("INTEGER"),
  /**
   * Returns a local date time field definition.
   * @returns A new field definition.
   */
  localDatetime: () => new FieldDef("LOCAL DATETIME"),
  /**
   * Returns a local time field definition.
   * @returns A new field definition.
   */
  localTime: () => new FieldDef("LOCAL TIME"),
  /**
   * Returns a map field definition.
   * @returns A new field definition.
   */
  map: () => new FieldDef("MAP"),
  /**
   * Returns a nothing or null field definition.
   * @returns A new field definition.
   */
  nothing: () => new FieldDef("NOTHING"),
  /**
   * Returns a path field definition (graph only).
   * @returns A new field definition.
   */
  path: () => new FieldDef("PATH"),
  /**
   * Returns a point field definition (graph only).
   * @returns A new field definition.
   */
  point: () => new FieldDef("POINT"),
  /**
   * Returns a string field definition.
   * @returns A new field definition.
   */
  string: () => new FieldDef("STRING"),
  /**
   * Returns a zoned date time field definition.
   * @returns A new field definition.
   */
  zonedDatetime: () => new FieldDef("ZONED DATETIME"),
  /**
   * Returns a zoned time field definition.
   * @returns A new field definition.
   */
  zonedTime: () => new FieldDef("ZONED TIME"),
};

/**
 * Utility for creating relationship definitions.
 */
export const relation = {
  /**
   * Creates a new relationship definition.
   * @param model The model to target.
   * @returns A new relationship definition.
   */
  to: <T extends string>(model: T) => new RelationshipDef(model),
};

/**
 * Defines a new node.
 * @param def The node schema.
 * @returns The same object, with types.
 */
export function model<TNode extends Node>(def: TNode): TNode {
  return def;
}

/**
 * Defines a new schema.
 * @param def The schema.
 * @returns The same object, with types.
 */
export function defineSchema<TSchema extends Schema>(def: TSchema): TSchema {
  return def;
}

export class Path {
  // TODO
}

export class Point {
  // TODO
}

export class Duration {
  // TODO
}
