import express from "express";
import {
  initializeRoadmap,
  updateRoadmap,
  getAllActiveRoadmaps,
  saveRoadmap,
  queryTransaction,
  queryAddressHistory,
  getAllCompletedRoadmaps,
  saveArchivedRoadmap,
  restoreRoadmap,
  getAllArchivedRoadmaps,
  deleteArchivedRoadmap,
} from "../controllers/roadmap.controller.js";

import { adminAuth } from "../middleware/adminAuth.js";

const router = express.Router();

router.get("/all", getAllActiveRoadmaps);
router.post("/update", updateRoadmap);
router.post("/initialize", initializeRoadmap);
router.post("/save", saveRoadmap);
router.post("/query/txs", queryTransaction);
router.post("/history/addr", queryAddressHistory);
router.get("/completed/all", getAllCompletedRoadmaps);

// To archived admin permission is required
router.post("/archive", adminAuth, saveArchivedRoadmap);
router.post("/restore/:id", adminAuth, restoreRoadmap);
router.get("/archived/all", getAllArchivedRoadmaps);
router.delete("/archived/:id", adminAuth, deleteArchivedRoadmap);

export default router;
