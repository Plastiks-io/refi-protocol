import express from "express";
import { sentPC } from "../controllers/user.controller.js";

const router = express.Router();

router.post("/send-pc", sentPC);

export default router;
