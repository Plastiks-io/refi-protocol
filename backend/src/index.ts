import express, { Request, Response } from "express";
import cors from "cors";
import cookieParser from "cookie-parser";
import http from "http";
import { Server as IOServer } from "socket.io";
import { initIO } from "./utils/socket.js";

// ✅ Import your environment configuration
import config from "./config/environment.js";

import sequelize from "./db/config.js";
import { seedSuperAdmin } from "./db/seed-super-admin.js";

import roadmapRoutes from "./routes/roadmap.routes.js";
import nftRoutes from "./routes/nft.routes.js";
import adminsRouter from "./routes/admin.routes.js";
import authRouter from "./routes/auth.routes.js";
import transactionRoutes from "./routes/transaction.routes.js";

// ESM "is this the main module?" check
import { fileURLToPath } from "url";
import path from "path";
const __filename = fileURLToPath(import.meta.url);

// Initialize your BullMQ queue & worker
import "./bull/index.js";

export async function bootstrap(): Promise<void> {
  // 1. Seed Super‑Admin
  try {
    await sequelize.authenticate();
    console.log("✅ Database connection OK");

    await sequelize.sync({ alter: true });
    console.log("✅ Models synced with database");

    await seedSuperAdmin();
  } catch (err) {
    console.error("❌ Database init failed:", err);
    process.exit(1);
  }

  // 2. Create Express app
  const app = express();
  const server = http.createServer(app);

  const io = new IOServer(server, {
    path: "/socket.io",
    cors: {
      origin: [config.FRONTEND_URL, config.FRONTEND_URL_2],
      methods: ["GET", "POST"],
      credentials: true,
    },
  });

  // Make `io` available in your routes/workers
  app.set("io", io);
  initIO(io);

  app.use(
    cors({
      origin: [config.FRONTEND_URL, config.FRONTEND_URL_2],
      credentials: true,
    })
  );
  app.use(cookieParser());
  app.use(express.json());

  // 3. Mount routes
  app.get("/", (_req: Request, res: Response) =>
    res.json({
      message: "Plastiks Backend API",
      environment: config.NODE_ENV,
      version: "1.0.0",
      timestamp: new Date().toISOString(),
    })
  );

  app.use("/roadmap", roadmapRoutes);
  app.use("/nft", nftRoutes);
  app.use("/admin", adminsRouter);
  app.use("/auth", authRouter);
  app.use("/transaction", transactionRoutes);

  // 4. Start server if run directly
  server.listen(config.PORT, () =>
    console.log(
      `🚀 Plastiks Backend Server running on port ${config.PORT} [${config.NODE_ENV}]`
    )
  );
}

bootstrap().catch((error) => {
  console.error("❌ Bootstrap failed:", error);
  process.exit(1);
});
