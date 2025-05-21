import { signIn, signOut } from "../controllers/auth.controller.js";
import express from "express";
const router = express.Router();

router.post("/signin", signIn);
router.post("/signout", signOut);

export default router;
