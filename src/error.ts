export const enum KineoErrorKind {}

export class KineoError extends Error {
  kind: KineoErrorKind;

  constructor(kind: KineoErrorKind, message?: string) {
    super(message ?? KineoError.getMessageFromKind(kind));
    this.kind = kind;
  }

  static getMessageFromKind(kind: KineoErrorKind) {
    switch (kind) {
      default:
        return "no message";
    }
  }
}

export const enum KineoKitErrorKind {
  MissingSchema = "MissingSchema",
  MissingClient = "MissingClient",
  NoSupport = "NoSupport",
  BreakingSchemaChange = "BreakingSchemaChange",
}

export class KineoKitError<T> extends Error {
  kind: KineoKitErrorKind;
  data?: T;

  constructor(kind: KineoKitErrorKind, data?: T, message?: string) {
    super(message ?? KineoKitError.getMessageFromKind(kind));
    this.kind = kind;
    this.data = data;
  }

  static getMessageFromKind(kind: KineoKitErrorKind) {
    switch (kind) {
      case KineoKitErrorKind.NoSupport:
        return "the adapter you're using doesn't support this function";
      case KineoKitErrorKind.MissingClient:
      case KineoKitErrorKind.MissingSchema:
        return `${kind === KineoKitErrorKind.MissingClient ? "client" : "schema"} is undefined. check if the file exists or if imports are resolving correctly`;
      case KineoKitErrorKind.BreakingSchemaChange:
        return "a breaking change was detected in the schema.";
      default:
        return "no message";
    }
  }
}
