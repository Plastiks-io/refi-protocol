import express from "express";
import {
  initializeRoadmap,
  updateRoadmap,
  getAllRoadmaps,
} from "../controllers/roadmap.controller.js";

const router = express.Router();

router.get("/all", getAllRoadmaps);
router.post("/update", updateRoadmap);
router.post("/initialize", initializeRoadmap);

export default router;
