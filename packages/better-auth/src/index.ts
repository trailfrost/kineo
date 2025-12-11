import type { Kineo } from "kineo";
import { createAdapterFactory } from "better-auth/adapters";

export const kineoAdapter = (client: Kineo<any, any>) =>
  createAdapterFactory({
    config: {
      adapterId: "@kineojs/better-auth",
    },
    adapter: () => ({
      // TODO
    }),
  });
