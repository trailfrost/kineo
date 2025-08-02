import Kineo from "kineo";
import schema from "./schema";

export default Kineo({
  url: "bolt://localhost:7687",
  auth: {
    username: "neo4j",
    password: "password",
  },
  schema,
});
