import axios from "axios";
import { Transaction, TransactionType } from "../models/transaction.model.js";
import { Request, Response } from "express";

interface TransactionAttributes {
  txDate: Date;
  txFee: number;
  amount: number;
  roadmapId: string;
  assetId: string;
  hash: string;
  type: TransactionType;
}

interface MultipleAssetTransactionAttributes {
  txDate: Date;
  txFee: number;
  plastikAmount: number;
  USDMAmount: number;
  roadmapId: string;
  plastikToken: string;
  usdmToken: string;
  hash: string;
  type1: TransactionType.Token;
  type2: TransactionType.Transfer;
}

// src/controllers/transaction.controller.ts

const getAllTransactions = async (req: Request, res: Response) => {
  try {
    // parse query params with defaults
    const page = Math.max(parseInt(req.query.page as string) || 1, 1);
    const perPage = Math.max(parseInt(req.query.perPage as string) || 10, 1);

    const offset = (page - 1) * perPage;

    // fetch rows + total count
    const { count, rows } = await Transaction.findAndCountAll({
      limit: perPage,
      offset,
      order: [["txDate", "DESC"]],
    });

    res.status(200).json({
      transactions: rows,
      meta: {
        total: count,
        page,
        perPage,
        totalPages: Math.ceil(count / perPage),
      },
      message: "Transactions fetched successfully",
    });
  } catch (error: any) {
    console.error("Error fetching transactions:", error);
    res.status(500).json({
      message: "Failed to fetch transactions",
      error: error.message,
    });
  }
};

const saveTransaction = async (req: Request, res: Response) => {
  try {
    const transactionAttributes: TransactionAttributes = req.body;
    await Transaction.create(transactionAttributes);
    res.status(200).json({
      message: "Transaction saved successfully for token return",
    });
  } catch (error) {
    console.error("Error saving transaction:", error);
    res.status(500).json({
      message: "Failed to save transaction",
      error: error instanceof Error ? error.message : "Unknown error",
    });
  }
};

const saveMultipleAssetTransactions = async (req: Request, res: Response) => {
  try {
    const multipleAssetTransactionAttributes: MultipleAssetTransactionAttributes =
      req.body;
    await Transaction.create({
      txDate: multipleAssetTransactionAttributes.txDate,
      txFee: multipleAssetTransactionAttributes.txFee,
      amount: multipleAssetTransactionAttributes.plastikAmount,
      roadmapId: multipleAssetTransactionAttributes.roadmapId,
      assetId: multipleAssetTransactionAttributes.plastikToken,
      hash: multipleAssetTransactionAttributes.hash,
      type: multipleAssetTransactionAttributes.type1 as TransactionType,
    });

    const precisionFactor = 1_000_000;
    const usdmAmount =
      multipleAssetTransactionAttributes.USDMAmount / precisionFactor;
    await Transaction.create({
      txDate: multipleAssetTransactionAttributes.txDate,
      txFee: multipleAssetTransactionAttributes.txFee,
      amount: usdmAmount,
      roadmapId: multipleAssetTransactionAttributes.roadmapId,
      assetId: multipleAssetTransactionAttributes.usdmToken,
      hash: multipleAssetTransactionAttributes.hash,
      type: multipleAssetTransactionAttributes.type2 as TransactionType,
    });

    res.status(200).json({
      message: "Transactions saved successfully for multiple assets",
    });
  } catch (error) {
    console.error("Error saving transactions:", error);
    res.status(500).json({
      message: "Failed to save transactions",
      error: error instanceof Error ? error.message : "Unknown error",
    });
  }
};

export { getAllTransactions, saveTransaction, saveMultipleAssetTransactions };
