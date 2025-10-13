import express from "express";

import {
  getAllTransactions,
  saveTransaction,
  saveMultipleAssetTransactions,
} from "../controllers/transaction.controller.js";
import { transactionUpdatedQueue } from "../bull/queues.js";

const router = express.Router();

// health check
router.get("/", (req, res) => {
  res.json({ status: "ok" });
});

router.get("/all", getAllTransactions);
router.post("/save", saveTransaction);
router.post("/save-multiple", saveMultipleAssetTransactions);

router.post("/updated", async (req, res) => {
  try {
    const payload = req.body;
    const job = await transactionUpdatedQueue.add(
      "transactionUpdatedQueue",
      payload
    );
    res.json({
      jobId: job.id,
      message: "Transaction updated processing started",
      success: true,
    });
  } catch (error) {
    console.error("Error saving transaction:", error);
    res.status(500).json({
      message: "Failed to save transaction",
      error: error instanceof Error ? error.message : "Unknown error",
    });
  }
});

export default router;
