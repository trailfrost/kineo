import type { ReactNode } from "react";
import clsx from "clsx";
import Heading from "@theme/Heading";
import styles from "./styles.module.css";

import docusaurusMountain from "@site/static/img/undraw_docusaurus_mountain.svg";
import docusaurusTree from "@site/static/img/undraw_docusaurus_tree.svg";
import docusaurusReact from "@site/static/img/undraw_docusaurus_react.svg";

type FeatureItem = {
  title: string;
  Svg: React.ComponentType<React.ComponentProps<"svg">>;
  description: ReactNode;
};

const FeatureList: FeatureItem[] = [
  {
    title: "Easy to Use",
    Svg: docusaurusMountain,
    description: (
      <>
        Kineo was designed to be familiar for TypeScript developers. Pass
        objects in, get objects out.
      </>
    ),
  },
  {
    title: "Pure TypeScript",
    Svg: docusaurusTree,
    description: (
      <>
        Kineo defines everything in TypeScript &mdash; from schema, to client
        definition, to queries.
      </>
    ),
  },
  {
    title: "Graph Database Compatible",
    Svg: docusaurusReact,
    description: (
      <>
        Kineo was built to work with graph databases, having utility methods for
        them.
      </>
    ),
  },
  {
    title: "Pluggable",
    Svg: docusaurusMountain,
    description: (
      <>
        Kineo can be extended to talk to any database you want &mdash; with
        official adapters for various databases.
      </>
    ),
  },
  {
    title: "Fast",
    Svg: docusaurusTree,
    description: (
      <>
        The only thing that bottlenecks speed in Kineo is the database latency
        itself.
      </>
    ),
  },
  {
    title: "Mostly Edge-Compatible",
    Svg: docusaurusReact,
    description: (
      <>
        Most, but not all built-in Kineo adapters are edge-compatible. It
        depends on the database driver used.
      </>
    ),
  },
];

function Feature({ title, Svg, description }: FeatureItem) {
  return (
    <div className={clsx("col col--4")}>
      <div className="text--center">
        <Svg className={styles.featureSvg} role="img" />
      </div>
      <div className="text--center padding-horiz--md">
        <Heading as="h3">{title}</Heading>
        <p>{description}</p>
      </div>
    </div>
  );
}

export default function HomepageFeatures(): ReactNode {
  return (
    <section className={styles.features}>
      <div className="container">
        <div className="row">
          {FeatureList.map((props, idx) => (
            <Feature key={idx} {...props} />
          ))}
        </div>
      </div>
    </section>
  );
}
