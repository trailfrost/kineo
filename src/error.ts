/**
 * Types of errors related to Kineo.
 */
export const enum KineoErrorKind {}

/**
 * A Kineo error.
 */
export class KineoError extends Error {
  /**
   * The type of error.
   */
  kind: KineoErrorKind;

  /**
   * Creates a new Kineo error.
   * @param kind The type of error.
   * @param message The message.
   */
  constructor(kind: KineoErrorKind, message?: string) {
    super(message ?? KineoError.getMessageFromKind(kind));
    this.kind = kind;
  }

  /**
   * Gets a message for a type of error.
   * @param kind The type.
   * @returns A message.
   */
  static getMessageFromKind(kind: KineoErrorKind) {
    switch (kind) {
      default:
        return "no message";
    }
  }
}

/**
 * A KineoKit (migration manager) related error.
 */
export const enum KineoKitErrorKind {
  MissingSchema = "MissingSchema",
  MissingClient = "MissingClient",
  NoSupport = "NoSupport",
  BreakingSchemaChange = "BreakingSchemaChange",
  FilePathNecessary = "FilePathNecessary",
}

/**
 * A KineoKit error.
 */
export class KineoKitError<T> extends Error {
  /**
   * The type of error.
   */
  kind: KineoKitErrorKind;
  /**
   * Associated data.
   */
  data?: T;

  /**
   * Creates a new KineoKit error.
   * @param kind The type of error.
   * @param data Associated data.
   * @param message The error message.
   */
  constructor(kind: KineoKitErrorKind, data?: T, message?: string) {
    super(message ?? KineoKitError.getMessageFromKind(kind));
    this.kind = kind;
    this.data = data;
  }

  /**
   * Gets a message for a type of error.
   * @param kind The kind.
   * @returns A message.
   */
  static getMessageFromKind(kind: KineoKitErrorKind) {
    switch (kind) {
      case KineoKitErrorKind.NoSupport:
        return "the adapter you're using doesn't support this function";
      case KineoKitErrorKind.MissingClient:
      case KineoKitErrorKind.MissingSchema:
        return `${kind === KineoKitErrorKind.MissingClient ? "client" : "schema"} is undefined. check if the file exists or if imports are resolving correctly`;
      case KineoKitErrorKind.BreakingSchemaChange:
        return "a breaking change was detected in the schema";
      case KineoKitErrorKind.FilePathNecessary:
        return "file path style imports are necessary for this action";
      default:
        return "no message";
    }
  }
}
