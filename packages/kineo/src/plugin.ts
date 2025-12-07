import type { OptPromise } from "./adapter";
import type { Kineo } from "./client";
import type { Model } from "./model";

export interface Plugin {
  init?(client: Kineo<any, any>): OptPromise<void>;

  onExec?(model: Model<any, any>, op: string): OptPromise<void>;
}
