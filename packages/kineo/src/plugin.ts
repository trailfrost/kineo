import type { OptPromise } from "./adapter";
import type { Kineo } from "./client";
import type { Model } from "./model";

/**
 * A Kineo plugin.
 */
export interface Plugin {
  /**
   * Runs when a plugin is being initialized.
   * @param client The initialized client.
   */
  init?(client: Kineo<any, any>): OptPromise<void>;

  /**
   * Runs when an operation is run on a model.
   * @param model The model the operation was executed on.
   * @param op The operation.
   */
  onExec?(model: Model<any, any>, op: string): OptPromise<void>;
}
