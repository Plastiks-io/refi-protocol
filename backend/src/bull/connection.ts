import { Redis } from "ioredis";
import config from "../config/environment.js";

export const connection = new Redis({
  host: config.REDIS.host,
  port: config.REDIS.port,
  maxRetriesPerRequest: null,
  enableReadyCheck: false,
});
