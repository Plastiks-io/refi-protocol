// test/user.test.ts
import * as userController from "../src/controllers/user.controller.js";
import { Lucid, TxComplete, TxSigned } from "lucid-cardano";
import { Transaction } from "../src/models/transaction.model.js";
import { jest } from "@jest/globals";
import * as dotenv from "dotenv";
dotenv.config();

describe("User Controllers Tests", () => {
  let fakeLucid: Partial<Lucid>;
  let req: any;
  let res: any;

  beforeEach(() => {
    // Reset req/res
    req = { body: {} };
    res = {
      status: jest.fn().mockReturnThis(),
      json: jest.fn(),
    };

    // 1) Stub out Lucid.new() → fakeLucid
    fakeLucid = {
      selectWalletFromSeed: jest.fn(),
      newTx: jest.fn(),
    };
    jest.spyOn(Lucid, "new").mockResolvedValue(fakeLucid as Lucid);

    // 2) Stub out Transaction.create so no DB errors
    jest.spyOn(Transaction, "create").mockResolvedValue({} as any);

    // 3) Silence console.error
    jest.spyOn(console, "error").mockImplementation(() => {});
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  it("should return 400 if address or amount are missing", async () => {
    // no req.body.address or amount
    await userController.sentPC(req, res);

    expect(res.status).toHaveBeenCalledWith(400);
    expect(res.json).toHaveBeenCalledWith({
      message: "Address and amount are required",
      success: false,
    });
  });

  it("should send PC token successfully and return 200", async () => {
    req.body = { address: "addr1...", amount: 1000 };

    // --- Build mock TxComplete with fee + sign chain ---
    const mockTxComplete: Partial<TxComplete> = {
      fee: 42n,
      sign: jest.fn().mockReturnThis(),
      complete: jest.fn().mockResolvedValue({
        submit: jest.fn().mockResolvedValue("fakeTxHash"),
      } as Partial<TxSigned>),
    };

    // Builder → complete() → mockTxComplete
    const mockBuilder = {
      payToAddress: jest.fn().mockReturnThis(),
      complete: jest.fn().mockResolvedValue(mockTxComplete),
    };
    (fakeLucid!.newTx as jest.Mock).mockReturnValue(mockBuilder as any);

    await userController.sentPC(req, res);

    // Assert happy‑path response
    expect(res.status).toHaveBeenCalledWith(200);
    expect(res.json).toHaveBeenCalledWith({
      message: "Transaction successful",
      txHash: "fakeTxHash",
      success: true,
    });

    // Assert DB write saw the correct fee
    expect(Transaction.create).toHaveBeenCalledWith(
      expect.objectContaining({ txFee: mockTxComplete.fee })
    );
  });

  it("should return 500 if submit() throws", async () => {
    req.body = { address: "addr1...", amount: 1000 };

    // Create a TxComplete whose .complete() succeeds, but whose .submit() fails
    const mockTxComplete: Partial<TxComplete> = {
      fee: 99n,
      sign: jest.fn().mockReturnThis(),
      complete: jest.fn().mockResolvedValue({
        submit: jest.fn().mockRejectedValue(new Error("oops")),
      } as Partial<TxSigned>),
    };
    const mockBuilder = {
      payToAddress: jest.fn().mockReturnThis(),
      complete: jest.fn().mockResolvedValue(mockTxComplete),
    };
    (fakeLucid!.newTx as jest.Mock).mockReturnValue(mockBuilder as any);

    await userController.sentPC(req, res);

    expect(res.status).toHaveBeenCalledWith(500);
    expect(res.json).toHaveBeenCalledWith({
      message: "Transaction failed",
      error: "oops",
    });
  });
});
