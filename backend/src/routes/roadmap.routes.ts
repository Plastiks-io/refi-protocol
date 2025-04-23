import express from "express";
import {
  initializeRoadmap,
  updateRoadmap,
  getAllRoadmaps,
  releaseFunds,
  queryTransaction,
  queryAddressHistory,
  getAllCompletedRoadmaps,
} from "../controllers/roadmap.controller.js";

const router = express.Router();

router.get("/all", getAllRoadmaps);
router.post("/update", updateRoadmap);
router.post("/initialize", initializeRoadmap);
router.post("/release", releaseFunds);
router.post("/query/txs", queryTransaction);
router.post("/history/addr", queryAddressHistory);

router.get("/completed/all", getAllCompletedRoadmaps);

export default router;
