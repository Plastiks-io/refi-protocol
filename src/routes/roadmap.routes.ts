import express from "express";
import {
  initializeRoadmap,
  updateRoadmap,
  getAllRoadmaps,
  releaseFunds,
  queryTransaction,
} from "../controllers/roadmap.controller.js";

const router = express.Router();

router.get("/all", getAllRoadmaps);
router.post("/update", updateRoadmap);
router.post("/initialize", initializeRoadmap);
router.post("/release", releaseFunds);
router.post("/query", queryTransaction);

export default router;
