import express from "express";

import {
  getAllTransactions,
  saveTransaction,
  saveMultipleAssetTransactions,
} from "../controllers/transaction.controller.js";

const router = express.Router();

router.get("/all", getAllTransactions);
router.post("/save", saveTransaction);
router.post("/save-multiple", saveMultipleAssetTransactions);

export default router;
