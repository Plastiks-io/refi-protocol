import { Redis } from "ioredis";
import config from "../config/environment.js";

export const connection = new Redis({
  host: config.REDIS.HOST,
  port: config.REDIS.PORT,
  maxRetriesPerRequest: null,
  enableReadyCheck: false,
});
