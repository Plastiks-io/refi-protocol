import express, { Request, Response } from "express";
import cors from "cors";
import cookieParser from "cookie-parser";
import dotenv from "dotenv";

import sequelize from "./db/config.js";
import { seedSuperAdmin } from "./db/seed-super-admin.js";

import roadmapRoutes from "./routes/roadmap.routes.js";
import userRoutes from "./routes/user.routes.js";
import adminsRouter from "./routes/admin.routes.js";
import authRouter from "./routes/auth.routes.js";

dotenv.config();

async function bootstrap() {
  // 1. Seed Super-Admin first
  try {
    await sequelize.authenticate();
    console.log("Database connection OK");

    // 2. Sync (migrations) & start Express
    await sequelize.sync({ alter: true });
    console.log("Models synced with database");

    await seedSuperAdmin();
  } catch (err) {
    console.error("Database init failed:", err);
    process.exit(1);
  }

  const app = express();
  const PORT = process.env.PORT || 8000;

  app.use(
    cors({
      origin: process.env.FRONTEND_URL || "http://localhost:3000",
      credentials: true,
    })
  );
  app.use(cookieParser());
  app.use(express.json());

  // 3. Mount routers
  app.get("/", (req: Request, res: Response) => {
    res.send("Hello, TypeScript with Express!");
  });
  app.use("/roadmap", roadmapRoutes);
  app.use("/user", userRoutes);

  // admin routes:
  app.use("/admin", adminsRouter);
  // auth routes:
  app.use("/auth", authRouter);

  app.listen(PORT, () =>
    console.log(`Server running on http://localhost:${PORT}`)
  );
}

bootstrap();
