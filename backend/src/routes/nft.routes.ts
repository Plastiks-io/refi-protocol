// src/routes/buy-nft.routes.ts
import express from "express";
import { buyNftQueue } from "../bull/queues.js";

const router = express.Router();
router.post("/buy", async (req, res) => {
  const payload = req.body;
  const job = await buyNftQueue.add("buyNftQueue", payload);
  res.json({
    jobId: job.id,
    message: "NFT buy successfully",
    success: true,
  });
});

export default router;
