import axios from "axios";
import { Transaction, TransactionType } from "../models/transaction.model.js";
import { Request, Response } from "express";

const getAllTransactions = async (req: Request, res: Response) => {
  try {
    const transactions = await Transaction.findAll();
    res.status(200).json({
      transactions,
      message: "All transactions fetched successfully",
    });
  } catch (error: any) {
    console.error("Error fetching all transactions:", error);
    res.status(500).json({
      message: "Failed to fetch all transactions",
      error: error.message,
    });
  }
};

export { getAllTransactions };
