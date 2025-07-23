import { describe, it, expect, vi, beforeEach } from "vitest";
import {
  Transaction,
  TransactionType,
} from "../../src/models/transaction.model";

describe("Transaction model tests (mocked)", () => {
  beforeEach(() => {
    vi.restoreAllMocks();
  });

  it("should create a Transaction with valid fields", async () => {
    const fakeTransaction = {
      id: "mock-tx-id",
      createdAt: new Date("2025-01-01T10:00:00Z"),
      type: TransactionType.Sold,
    };

    vi.spyOn(Transaction, "create").mockResolvedValue(fakeTransaction as any);

    const tx = await Transaction.create({
      txDate: new Date("2025-01-01T10:00:00Z"),
      txFee: 1.5,
      amount: 100,
      roadmapId: "roadmap123",
      assetId: "asset456",
      hash: "0xabc123def456",
      type: TransactionType.Sold,
    });

    expect(tx.id).toBe("mock-tx-id");
    expect(tx.createdAt).toBeInstanceOf(Date);
    expect(tx.type).toBe(TransactionType.Sold);
  });

  it("should throw error when required fields are missing", async () => {
    vi.spyOn(Transaction, "create").mockRejectedValue(
      new Error("Missing required field: txDate")
    );

    await expect(
      Transaction.create({
        txFee: 0.5,
        amount: 50,
        roadmapId: "rm001",
        assetId: "as001",
        hash: "0xmissingDate",
        type: TransactionType.Token,
      } as any)
    ).rejects.toThrow("Missing required field: txDate");
  });

  it("should throw error for invalid type value", async () => {
    vi.spyOn(Transaction, "create").mockRejectedValue(
      new Error("Invalid transaction type: invalidType")
    );

    await expect(
      Transaction.create({
        txDate: new Date(),
        txFee: 0.5,
        amount: 50,
        roadmapId: "rm002",
        assetId: "as002",
        hash: "0xinvalidEnum",
        type: "invalidType" as any,
      })
    ).rejects.toThrow("Invalid transaction type: invalidType");
  });

  it("should generate id and createdAt by default", async () => {
    const fakeTransaction = {
      id: "123e4567-e89b-12d3-a456-426614174000",
      createdAt: new Date(),
    };

    vi.spyOn(Transaction, "create").mockResolvedValue(fakeTransaction as any);

    const tx = await Transaction.create({
      txDate: new Date(),
      txFee: 2.5,
      amount: 150,
      roadmapId: "rm004",
      assetId: "as004",
      hash: "0xautoFields",
      type: TransactionType.Roadmap,
    });

    expect(tx.id).toMatch(
      /^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i
    );
    expect(tx.createdAt).toBeInstanceOf(Date);
  });

  it("should allow past and future dates for txDate", async () => {
    const pastTransaction = {
      txDate: new Date("2000-01-01"),
    };
    const futureTransaction = {
      txDate: new Date("2100-01-01"),
    };

    const mockCreate = vi
      .spyOn(Transaction, "create")
      .mockResolvedValueOnce(pastTransaction as any)
      .mockResolvedValueOnce(futureTransaction as any);

    const past = await Transaction.create({
      txDate: new Date("2000-01-01"),
      txFee: 0.5,
      amount: 100,
      roadmapId: "rmPast",
      assetId: "asPast",
      hash: "0xPastHash",
      type: TransactionType.Transfer,
    });

    const future = await Transaction.create({
      txDate: new Date("2100-01-01"),
      txFee: 1.2,
      amount: 200,
      roadmapId: "rmFuture",
      assetId: "asFuture",
      hash: "0xFutureHash",
      type: TransactionType.Transfer,
    });

    expect(past.txDate.getFullYear()).toBe(2000);
    expect(future.txDate.getFullYear()).toBe(2100);
    expect(mockCreate).toHaveBeenCalledTimes(2);
  });
});
