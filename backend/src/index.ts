import express, { Request, Response } from "express";
import cors from "cors";
import cookieParser from "cookie-parser";
import dotenv from "dotenv";
dotenv.config();

import sequelize from "./db/config.js";
import { seedSuperAdmin } from "./db/seed-super-admin.js";

import roadmapRoutes from "./routes/roadmap.routes.js";
import nftRoutes from "./routes/nft.routes.js";
import adminsRouter from "./routes/admin.routes.js";
import authRouter from "./routes/auth.routes.js";
import transactionRoutes from "./routes/transaction.routes.js";

// ESM “is this the main module?” check
import { fileURLToPath } from "url";
import path from "path";
const __filename = fileURLToPath(import.meta.url);

// Initialize your BullMQ queue & worker
import "./bull.js";

export async function bootstrap(): Promise<void> {
  // 1. Seed Super‑Admin
  try {
    await sequelize.authenticate();
    console.log("Database connection OK");

    await sequelize.sync({ alter: true });
    console.log("Models synced with database");

    await seedSuperAdmin();
  } catch (err) {
    console.error("Database init failed:", err);
    process.exit(1);
  }

  // 2. Create Express app
  const app = express();
  const PORT = Number(process.env.PORT) || 8000;

  app.use(
    cors({
      origin: process.env.FRONTEND_URL || "http://localhost:3000",
      credentials: true,
    })
  );
  app.use(cookieParser());
  app.use(express.json());

  // 3. Mount routes
  app.get("/", (_req: Request, res: Response) =>
    res.send("Hello, TypeScript with Express!")
  );
  app.use("/roadmap", roadmapRoutes);
  app.use("/nft", nftRoutes);
  app.use("/admin", adminsRouter);
  app.use("/auth", authRouter);
  app.use("/transaction", transactionRoutes);

  // 4. Start server if run directly
  if (process.argv[1] === __filename) {
    app.listen(PORT, () =>
      console.log(`🚀 Server listening on http://localhost:${PORT}`)
    );
  }
}

bootstrap();
