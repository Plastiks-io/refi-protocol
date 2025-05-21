import {
  createAdmin,
  deleteAdmin,
  getAllAdmins,
  getAdminById,
} from "../controllers/admin.controller.js";
import { adminAuth } from "../middleware/adminAuth.js";
import express from "express";

const router = express.Router();

// Private admin endpoints
router.post("/", adminAuth, createAdmin);
router.delete("/:id", adminAuth, deleteAdmin);
router.get("/all", adminAuth, getAllAdmins);
router.get("/:id", adminAuth, getAdminById);

export default router;
