import * as userController from "../src/controllers/user.controller.js";
import { jest } from "@jest/globals";
import { Lucid, UTxO, Data, Constr, fromText, Tx } from "lucid-cardano";
import * as dotenv from "dotenv";
dotenv.config();

// Explicitly type fakeLucid as a partial Lucid instance with required methods
const fakeLucid: Pick<
  Lucid,
  "utxosAt" | "wallet" | "provider" | "utils" | "selectWalletFromSeed" | "newTx"
> = {
  utxosAt: jest.fn<() => Promise<UTxO[]>>().mockResolvedValueOnce([]),
  wallet: {
    address: async () => "someAddress",
  } as any, // Safe if only used in test
  provider: {
    getUtxosByOutRef: jest.fn<() => Promise<UTxO[]>>(),
  } as any,
  utils: {
    getAddressDetails: () => ({
      paymentCredential: {
        hash: "b93e78824bcf5c34a62b2f573727b4bb8a1365ebd152bd6243ff8dc6",
      },
    }),
    validatorToAddress: () => "fakeRefiAddress",
    keyHashToCredential: () => ({
      type: "Key",
      hash: "mockKeyHash",
    }),
    credentialToAddress: () => "fakeAddress",
  } as any,
  selectWalletFromSeed: () => fakeLucid as Lucid,
  newTx: jest.fn<() => Tx>(),
};

describe("User Controllers Tests", () => {
  let req: any, res: any;
  beforeEach(() => {
    req = {
      body: {},
    };

    res = {
      status: jest.fn().mockReturnThis(),
      json: jest.fn(),
    };

    jest.clearAllMocks();
    jest.spyOn(console, "error").mockImplementation(() => {});
    jest.spyOn(Lucid, "new").mockResolvedValue(fakeLucid as unknown as Lucid);
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });
  describe("sent PC token successfully", () => {
    it("should send PC token successfully and return 200", async () => {
      req.body = {
        address: "someAddress",
        amount: 1000,
      };
      const pcAssetId = process.env.PC_ASSET_ID! || "fakePcAssetId";
      // Mock transaction flow
      const tx = {
        payToAddress: jest.fn().mockReturnThis(),
        complete: jest.fn().mockReturnThis(),
        sign: jest.fn().mockReturnThis(),
        submit: jest.fn().mockResolvedValue("fakeTxHash" as unknown as never),
      };
      jest.spyOn(fakeLucid, "newTx").mockReturnValue(tx as unknown as Tx);

      await userController.sentPC(req, res);

      expect(res.status).toHaveBeenCalledWith(200);
      expect(res.json).toHaveBeenCalledWith({
        message: "Transaction successful",
        txHash: "fakeTxHash",
        success: true,
      });
    });

    it("should return 400 if adress or amount is not found", async () => {
      req.body = {};
      await userController.sentPC(req, res);
      expect(res.status).toHaveBeenCalledWith(400);
      expect(res.json).toHaveBeenCalledWith({
        message: "Address and amount are required",
        success: false,
      });
    });

    it("should handle errors and return 500", async () => {
      req.body = {
        address: "someAddress",
        amount: 1000,
      };
      // Mock transaction flow to throw an error
      const tx = {
        payToAddress: jest.fn().mockReturnThis(),
        complete: jest.fn().mockReturnThis(),
        sign: jest.fn().mockReturnThis(),
        submit: jest
          .fn()
          .mockRejectedValue(
            new Error("Transaction failed") as unknown as never
          ),
      };
      jest.spyOn(fakeLucid, "newTx").mockReturnValue(tx as unknown as Tx);
      await userController.sentPC(req, res);
      expect(res.status).toHaveBeenCalledWith(500);
      expect(res.json).toHaveBeenCalledWith({
        message: "Transaction failed",
        error: "Transaction failed",
      });
    });
  });
});
