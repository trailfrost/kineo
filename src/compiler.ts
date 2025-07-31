import type { IR } from "./ir";

export default function compile(ir: IR) {
  console.log("compiling IR:", ir);
  // TODO
  return {
    cypher: `
      MATCH (n)
      DETACH DELETE n
    `,
    params: {},
  };
}
