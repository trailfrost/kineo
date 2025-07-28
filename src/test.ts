import { defineSchema, node, field, relation } from "./schema";

const schema = defineSchema({
  Post: node({
    id: field("INTEGER").id(),
    title: field("STRING").required(),
    author: relation("User").incoming("HAS_POST").array(),
  }),

  User: node({
    name: field("STRING").id(),
    password: field("STRING").id(),
    posts: relation("Post").outgoing("HAS_POST"),
  }),
});
