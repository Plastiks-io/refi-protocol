import express from "express";
import { getGovernanceStats } from "../controllers/governance.controller.js";

const router = express.Router();

router.get("/stats", getGovernanceStats);

export default router;
