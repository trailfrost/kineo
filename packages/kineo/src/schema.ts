// TODO path, point, duration

/**
 * A path.
 */
export type Path = {};

/**
 * A point.
 */
export type Point = {};

/**
 * A duration.
 */
export type Duration = {};

/**
 * All supported Cypher types.
 */
export type CypherType =
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
  FieldDef<CypherType> | RelationshipDef<string>
>;

/**
 * Type for a schema.
 */
export type Schema = Record<string, Node>;

/**
 * Converts a Cypher type to a TypeScript type.
 */
type CypherToTs<T extends CypherType> = T extends "ANY"
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
 * Infers a value from a Cypher type.
 */
type InferValue<
  T extends CypherType,
  IsRequired extends boolean,
  IsArray extends boolean,
> = IsArray extends true
  ? Array<CypherToTs<T>>
  : IsRequired extends true
    ? CypherToTs<T>
    : CypherToTs<T> | undefined;

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
  TType extends CypherType,
  IsRequired extends boolean = false,
  IsArray extends boolean = false,
  IsId extends boolean = false,
> {
  /**
   * Cypher type this field maps to.
   */
  cypherType: TType;
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
   * @param type The Cypher type of this field.
   */
  constructor(type: TType) {
    this.cypherType = type;
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
 * Creates a field definition.
 * @param type The field type.
 * @returns A new field definition.
 */
export function field<T extends CypherType>(type: T): FieldDef<T> {
  return new FieldDef(type);
}

/**
 * Creates a new relationship definition.
 * @param to The node this is referencing.
 * @returns A new relationship definition.
 */
export function relation<T extends string>(to: T): RelationshipDef<T> {
  return new RelationshipDef(to);
}

/**
 * Defines a new node.
 * @param def The node schema.
 * @returns The same object, with types.
 */
export function node<TNode extends Node>(def: TNode): TNode {
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
