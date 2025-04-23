import express, { Request, Response } from "express";
import dotenv from "dotenv";
import cors from "cors";
import sequelize from "./db/config.js";
dotenv.config();

const app = express();
const PORT = process.env.PORT || 8000;

app.get("/", (req: Request, res: Response) => {
  res.send("Hello, TypeScript with Express!");
});
app.use(cors());
app.use(express.json());
import roadmapRoutes from "./routes/roadmap.routes.js";
app.use("/roadmap", roadmapRoutes);

import userRoutes from "./routes/user.routes.js";
app.use("/user", userRoutes);

// Sync the models with the database (create tables if not exists)
sequelize.sync({ alter: true }).then(() => {
  console.log("Database synced with changes!");
  app.listen(PORT, () => {
    console.log(`Server is running on http://localhost:${PORT}`);
  });
});

export default app;
