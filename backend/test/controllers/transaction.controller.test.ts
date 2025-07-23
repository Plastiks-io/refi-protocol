import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { Request, Response } from "express";
import {
  saveTransaction,
  saveMultipleAssetTransactions,
} from "../../src/controllers/transaction.controller";
import { Transaction } from "../../src/models/transaction.model";
import { updateRefiContractQueue } from "../../src/bull/queues";
import type { Job } from "bullmq";

// 🧪 Safe, strict mocks — NO DB interaction
vi.mock("../../src/models/transaction.model", () => ({
  Transaction: {
    create: vi.fn(),
  },
  TransactionType: {
    Token: "Token",
    Transfer: "Transfer",
  },
}));

vi.mock("../../src/bull/queues", () => ({
  updateRefiContractQueue: {
    add: vi.fn(),
  },
}));

describe("Transaction Controller (mocked DB)", () => {
  let req: Partial<Request>;
  let res: Partial<Response>;
  let statusMock: any;
  let jsonMock: any;

  beforeEach(() => {
    jsonMock = vi.fn();
    statusMock = vi.fn().mockReturnValue({ json: jsonMock });
    req = {};
    res = {
      status: statusMock,
      json: jsonMock,
    };
    vi.clearAllMocks();
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  describe("saveTransaction", () => {
    it("should call Transaction.create with payload and respond 200", async () => {
      const payload = {
        txDate: new Date(),
        txFee: 1,
        amount: 100,
        roadmapId: "roadmap1",
        assetId: "asset1",
        hash: "hash123",
        type: "Token",
      };

      req.body = payload;
      vi.mocked(Transaction.create).mockResolvedValue({});

      await saveTransaction(req as Request, res as Response);

      expect(Transaction.create).toHaveBeenCalledWith(payload);
      expect(statusMock).toHaveBeenCalledWith(200);
      expect(jsonMock).toHaveBeenCalledWith({
        message: "Transaction saved successfully for token return",
      });
    });

    it("should handle database create error", async () => {
      const payload = {
        txDate: new Date(),
        txFee: 1,
        amount: 100,
        roadmapId: "roadmap1",
        assetId: "asset1",
        hash: "hash123",
        type: "Token",
      };

      req.body = payload;
      vi.mocked(Transaction.create).mockRejectedValue(
        new Error("Insert failed")
      );

      await saveTransaction(req as Request, res as Response);

      expect(statusMock).toHaveBeenCalledWith(500);
      expect(jsonMock).toHaveBeenCalledWith({
        message: "Failed to save transaction",
        error: "Insert failed",
      });
    });
  });

  describe("saveMultipleAssetTransactions", () => {
    it("should enqueue refi job and save two transactions", async () => {
      const mockJob = { id: "job123" } as Job;
      const payload = {
        txDate: new Date(),
        txFee: 1,
        plastikAmount: 500,
        USDMAmount: 1_000_000,
        preId: "pre1",
        roadmapId: "road1",
        plastikToken: "plastikToken1",
        usdmToken: "usdmToken1",
        hash: "txHash123",
        type1: "Token",
        type2: "Transfer",
      };

      req.body = payload;
      vi.mocked(updateRefiContractQueue.add).mockResolvedValue(mockJob);
      vi.mocked(Transaction.create).mockResolvedValue({});

      await saveMultipleAssetTransactions(req as Request, res as Response);

      expect(updateRefiContractQueue.add).toHaveBeenCalledWith(
        "updateRefiContractQueue",
        {
          preId: payload.preId,
          roadmapId: payload.roadmapId,
          txHash: payload.hash,
        }
      );

      expect(Transaction.create).toHaveBeenCalledTimes(2);
      expect(statusMock).toHaveBeenCalledWith(200);
      expect(jsonMock).toHaveBeenCalledWith({
        message: "Transactions saved successfully for multiple assets",
        jobId: "job123",
      });
    });

    it("should handle failure in one of the saves", async () => {
      const mockJob = { id: "job123" } as Job;
      const payload = {
        txDate: new Date(),
        txFee: 1,
        plastikAmount: 500,
        USDMAmount: 1_000_000,
        preId: "pre1",
        roadmapId: "road1",
        plastikToken: "plastikToken1",
        usdmToken: "usdmToken1",
        hash: "txHash123",
        type1: "Token",
        type2: "Transfer",
      };

      req.body = payload;
      vi.mocked(updateRefiContractQueue.add).mockResolvedValue(mockJob);
      vi.mocked(Transaction.create).mockRejectedValue(
        new Error("Partial insert failed")
      );

      await saveMultipleAssetTransactions(req as Request, res as Response);

      expect(statusMock).toHaveBeenCalledWith(500);
      expect(jsonMock).toHaveBeenCalledWith({
        message: "Failed to save transactions",
        error: "Partial insert failed",
      });
    });
  });
});
