import type { CleanedWhere, JoinConfig } from "better-auth/adapters";
import * as IR from "kineo/ir";

export interface CompileOpts {
  model: string;
  where?: CleanedWhere[];
  select?: string[];
  limit?: number;
  sortBy?: {
    field: string;
    direction: "asc" | "desc";
  };
  offset?: number;
  data?: Record<string, any>;
  join?: JoinConfig;
}

export function compile(mode: string, opts: CompileOpts): IR.IR {
  // TODO convert Better-Auth IR to Kineo IR
  // maybe by converting based on mode to a query object first, and letting the IR do the work
  return {
    statements: [],
  };
}
