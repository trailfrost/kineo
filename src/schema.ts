import { Path, Point, Duration } from "neo4j-driver";

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

export type Direction = "IN" | "OUT" | "BOTH";

export type Node = Record<
  string,
  FieldDef<CypherType> | RelationshipDef<string>
>;
export type Schema = Record<string, Node>;

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

type InferValue<
  T extends CypherType,
  IsRequired extends boolean,
  IsArray extends boolean,
> = IsArray extends true
  ? Array<CypherToTs<T>>
  : IsRequired extends true
    ? CypherToTs<T>
    : CypherToTs<T> | undefined;

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

export type InferNode<
  TNode extends Node,
  TSchema extends Schema = { dummy: { dummy: RelationshipDef<"UNKNOWN"> } },
> = {
  [key in keyof TNode]: TNode[key] extends FieldDef<
    infer Type,
    infer Required extends boolean,
    infer Array extends boolean
  >
    ? InferValue<Type, Required, Array>
    : TNode[key] extends RelationshipDef<
          infer To extends string,
          infer Required extends boolean,
          infer Array extends boolean
        >
      ? InferRelationship<TSchema, To, Required, Array>
      : never;
};

export type InferSchema<T extends Schema> = {
  [key in keyof T]: InferNode<T[key], T>;
};

export class FieldDef<
  TType extends CypherType,
  IsRequired extends boolean = false,
  IsArray extends boolean = false,
  IsId extends boolean = false,
> {
  cypherType: TType;
  defaultValue: unknown | null;
  isPrimaryKey: boolean = false;
  isRequired: boolean = false;
  isUnique: boolean = false;
  isArray: boolean = false;

  constructor(type: TType) {
    this.cypherType = type;
  }

  id() {
    this.isPrimaryKey = true;
    this.isRequired = true;
    return this as FieldDef<TType, true, IsArray, true>;
  }

  default(value: unknown) {
    this.defaultValue = value;
    this.isRequired = true;
    return this as FieldDef<TType, false, IsArray, IsId>;
  }

  required() {
    this.isRequired = true;
    return this as FieldDef<TType, true, IsArray, IsId>;
  }

  optional() {
    this.isRequired = false;
    return this as FieldDef<TType, false, IsArray, IsId>;
  }

  array() {
    this.isArray = true;
    return this as FieldDef<TType, IsRequired, true, IsId>;
  }
}

export class RelationshipDef<
  To extends string,
  IsRequired extends boolean = false,
  IsArray extends boolean = false,
  Metadata extends object | undefined = undefined,
> {
  refTo: To;
  refLabel: string = "UNNAMED";
  refDirection: Direction = "OUT";
  isRequired: boolean = false;
  isArray: boolean = false;
  defaultValue: unknown | null;
  metadata: Metadata = undefined as Metadata;

  constructor(to: To) {
    this.refTo = to;
  }

  to(refTo: string) {
    this.refTo = refTo as To;
    return this as RelationshipDef<typeof refTo, IsRequired, IsArray, Metadata>;
  }

  label(name: string) {
    this.refLabel = name;
    return this;
  }

  outgoing(label: string) {
    this.refDirection = "OUT";
    this.refLabel = label;
    return this;
  }

  incoming(label: string) {
    this.refDirection = "IN";
    this.refLabel = label;
    return this;
  }

  both(label: string) {
    this.refDirection = "BOTH";
    this.refLabel = label;
    return this;
  }

  required() {
    this.isRequired = true;
    return this as RelationshipDef<To, true, IsArray, Metadata>;
  }

  default(value: unknown) {
    this.defaultValue = value;
    return this as RelationshipDef<To, false, IsArray, Metadata>;
  }

  array() {
    this.isArray = true;
    return this as RelationshipDef<To, IsRequired, true, Metadata>;
  }

  meta<T extends object>(metadata: T) {
    this.metadata = metadata as unknown as Metadata;
    return this as unknown as RelationshipDef<To, IsRequired, IsArray, T>;
  }
}

export function field<T extends CypherType>(type: T): FieldDef<T> {
  return new FieldDef(type);
}

export function relation<T extends string>(to: T): RelationshipDef<T> {
  return new RelationshipDef(to);
}

export function node<TNode extends Node>(def: TNode): TNode {
  return def;
}

export function defineSchema<TSchema extends Schema>(def: TSchema): TSchema {
  return def;
}

export { Duration, Point, Path };
