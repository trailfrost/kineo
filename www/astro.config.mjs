// @ts-check
import { defineConfig } from "astro/config";
import starlight from "@astrojs/starlight";

// https://astro.build/config
export default defineConfig({
  integrations: [
    starlight({
      title: "Kineo",
      social: [
        {
          icon: "github",
          label: "GitHub",
          href: "https://github.com/trailfrost/kineo",
        },
      ],
      sidebar: [{ label: "Guides", autogenerate: { directory: "guides" } }],
    }),
  ],
});
