import express from "express";
import {
  initializeRoadmap,
  getAllActiveRoadmaps,
  saveRoadmap,
  getAllCompletedRoadmaps,
  saveArchivedRoadmap,
  restoreRoadmap,
  getAllArchivedRoadmaps,
  deleteArchivedRoadmap,
} from "../controllers/roadmap.controller.js";

import { adminAuth } from "../middleware/adminAuth.js";
import { updateStakeContractQueue } from "../bull/queues.js";

const router = express.Router();

router.get("/all", getAllActiveRoadmaps);
router.post("/initialize", initializeRoadmap);
router.post("/save", saveRoadmap);
router.get("/completed/all", getAllCompletedRoadmaps);

// To archived admin permission is required
router.post("/archive", adminAuth, saveArchivedRoadmap);
router.post("/restore/:id", adminAuth, restoreRoadmap);
router.get("/archived/all", getAllArchivedRoadmaps);
router.delete("/archived/:id", adminAuth, deleteArchivedRoadmap);

// For updating stake contract
router.post("/enqueue-stake-check", async (req, res) => {
  const { txHash } = req.body;
  if (!txHash) return res.status(400).send("txHash required");
  const job = await updateStakeContractQueue.add("updateStakeContract", {
    txHash,
  });

  res.status(200).json({
    job: job.id,
    message: "Job added to queue",
    success: true,
  });
});

export default router;
