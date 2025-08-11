import { Sequelize } from "sequelize";
import config from "../config/environment.js";

const sequelize = new Sequelize(
  config.DATABASE.NAME!,
  config.DATABASE.USER!,
  config.DATABASE.PASSWORD!,
  {
    host: config.DATABASE.HOST,
    port: config.DATABASE.PORT,
    dialect: "postgres",

    logging: (msg) => {
      if (msg.toLowerCase().includes("error")) {
        console.error("Sequelize Error:", msg);
      }
    },
    pool: {
      max: config.NODE_ENV === "production" ? 20 : 5,
      min: 0,
      acquire: 30000,
      idle: 10000,
    },
  }
);

export default sequelize;
