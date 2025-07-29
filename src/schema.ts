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
  | "NODE"
  | "NOTHING"
  | "PATH"
  | "POINT"
  | "PROPERTY VALUE"
  | "RELATIONSHIP"
  | "STRING"
  | "ZONED DATETIME"
  | "ZONED TIME";

export type Direction = "IN" | "OUT" | "BOTH";

export type Node = Record<string, FieldDef | RelationshipDef>;
export type Schema = Record<string, Node>;

// TODO `InferSchema`/`InferNode` type

export class FieldDef {
  cypherTypes: CypherType[];
  defaultValue: unknown | null;
  isPrimaryKey: boolean = false;
  isRequired: boolean = false;
  isUnique: boolean = false;
  isArray: boolean = false;

  constructor(...types: CypherType[]) {
    this.cypherTypes = types;
  }

  id() {
    this.isPrimaryKey = true;
    this.isRequired = true;
    return this;
  }

  types(...types: CypherType[]) {
    this.cypherTypes.concat(types);
    return this;
  }

  default(value: unknown) {
    this.defaultValue = value;
    this.isRequired = true;
    return this;
  }

  required() {
    this.isRequired = true;
    return this;
  }

  optional() {
    this.isRequired = false;
    return this;
  }

  array() {
    this.isArray = true;
    return this;
  }
}

export class RelationshipDef {
  refTo: string;
  refLabel: string = "UNNAMED";
  refDirection: Direction = "OUT";
  isRequired: boolean = false;
  isArray: boolean = false;
  defaultValue: unknown | null = null;

  constructor(to: string) {
    this.refTo = to;
  }

  to(refTo: string) {
    this.refTo = refTo;
    return this;
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
    this.refDirection = "OUT";
    this.refLabel = label;
    return this;
  }

  direction(dir: Direction) {
    this.refDirection = dir;
    return this;
  }

  required() {
    this.isRequired = true;
    return this;
  }

  default(value: unknown) {
    this.defaultValue = value;
    return this;
  }

  array() {
    this.isArray = true;
    return this;
  }
}

export function field(...types: CypherType[]): FieldDef {
  return new FieldDef(...types);
}

export function relation(to: string): RelationshipDef {
  return new RelationshipDef(to);
}

export function node<TNode extends Node>(def: TNode): TNode {
  return def;
}

export function defineSchema<TSchema extends Schema>(def: TSchema): TSchema {
  return def;
}
