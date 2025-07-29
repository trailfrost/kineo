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
  ? Array<InferNode<T, T[To]>>
  : IsRequired extends true
    ? InferNode<T, T[To]>
    : InferNode<T, T[To]> | undefined;

type InferNode<T extends Schema, TNode extends Node> = {
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
      ? InferRelationship<T, To, Required, Array>
      : never;
};

export type InferSchema<T extends Schema> = {
  [key in keyof T]: InferNode<T, T[key]>;
};

export class FieldDef<
  TType extends CypherType,
  IsRequired extends boolean = false,
  IsArray extends boolean = false,
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
    return this as FieldDef<TType, true, IsArray>;
  }

  default(value: unknown) {
    this.defaultValue = value;
    this.isRequired = true;
    return this as FieldDef<TType, false, IsArray>;
  }

  required() {
    this.isRequired = true;
    return this as FieldDef<TType, true, IsArray>;
  }

  optional() {
    this.isRequired = false;
    return this as FieldDef<TType, false, IsArray>;
  }

  array() {
    this.isArray = true;
    return this as FieldDef<TType, IsRequired, true>;
  }
}

export class RelationshipDef<
  To extends string,
  IsRequired extends boolean = false,
  IsArray extends boolean = false,
> {
  refTo: To;
  refLabel: string = "UNNAMED";
  refDirection: Direction = "OUT";
  isRequired: boolean = false;
  isArray: boolean = false;
  defaultValue: unknown | null = null;

  constructor(to: To) {
    this.refTo = to;
  }

  to(refTo: string) {
    this.refTo = refTo as To;
    return this as RelationshipDef<typeof refTo, IsRequired, IsArray>;
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
    return this as RelationshipDef<To, true, IsArray>;
  }

  default(value: unknown) {
    this.defaultValue = value;
    return this as RelationshipDef<To, false, IsArray>;
  }

  array() {
    this.isArray = true;
    return this as RelationshipDef<To, IsRequired, true>;
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
