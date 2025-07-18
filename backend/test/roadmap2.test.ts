import * as roadmapController from "../src/controllers/roadmap.controller.js";
import * as StakeRewardController from "../src/controllers/stakeReward.controller.js";
import { jest } from "@jest/globals";
import CompletedRoadmap from "../src/models/completedRoadmap.model.js";
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

describe("Roadmap Controllers Tests", () => {
  let req: any, res: any;
  beforeEach(() => {
    req = {
      body: {},
      roadmap: {},
    };

    res = {
      status: jest.fn().mockReturnThis(),
      json: jest.fn(),
    };

    jest.clearAllMocks();
    jest.spyOn(console, "error").mockImplementation(() => {});
    jest.spyOn(Lucid, "new").mockResolvedValue(fakeLucid as unknown as Lucid);
    jest
      .spyOn(StakeRewardController, "getPubKeyHash")
      .mockResolvedValue(
        "deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"
      );
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  describe("initializeRoadmap", () => {
    it("should return 400 if required fields are missing", async () => {
      await roadmapController.initializeRoadmap(req, res);
      expect(res.status).toHaveBeenCalledWith(400);
      expect(res.json).toHaveBeenCalledWith({
        error: "Missing required fields",
      });
    });

    // check if roadmap already exist in completed roadmaps
    it("should return 409 if roadmap already exists", async () => {
      req.body = {
        preId: "pre1",
        roadmapId: "roadmap1",
        roadmapName: "CleanCoast Mission",
        roadmapDescription:
          "Focused on removing plastic waste from coastal regions and supporting local recycling units.",
        prePkh: "b93e78824bcf5c34a62b2f573727b4bb8a1365ebd152bd6243ff8dc6",
        preSkh: "a786470d2a2c8bc00ecaf662a64407364be25325f33d1cb9446b4bd7",
        totalPlasticCredits: 100,
        totalPlasticTokens: 10000,
        totalPlastic: 100,
      };

      // Properly mock CompletedRoadmap.findOne
      jest
        .spyOn(CompletedRoadmap, "findOne")
        .mockResolvedValueOnce({ ...req.body });

      await roadmapController.initializeRoadmap(req, res);

      expect(res.status).toHaveBeenCalledWith(409);
      expect(res.json).toHaveBeenCalledWith({
        message: "Roadmap already exists in completed roadmaps",
        success: false,
      });
    });
    // check if roadmap already exist in smart contract datum
    it("should return 409 if roadmap already exists in smart contract datum", async () => {
      req.body = {
        preId: "pre8",
        roadmapId: "roadmap8",
        roadmapName: "CleanCoast Mission",
        roadmapDescription:
          "Focused on removing plastic waste from coastal regions and supporting local recycling units.",
        prePkh: "b93e78824bcf5c34a62b2f573727b4bb8a1365ebd152bd6243ff8dc6",
        preSkh: "a786470d2a2c8bc00ecaf662a64407364be25325f33d1cb9446b4bd7",
        totalPlasticCredits: 100,
        totalPlasticTokens: 10000,
        totalPlastic: 100,
      };
      const fakeUtxos: UTxO[] = [
        {
          txHash: "fakeHash123",
          outputIndex: 0,
          address: "fakeRefiAddress",
          assets: {},
          datum: Data.to(
            new Constr(0, [fromText("pre8"), fromText("roadmap8")])
          ),
        },
      ];
      // Mock Lucid to return UTxOs with matching datum
      (fakeLucid.utxosAt as jest.Mock).mockResolvedValue(
        fakeUtxos as unknown as never
      );

      await roadmapController.initializeRoadmap(req, res);

      expect(res.status).toHaveBeenCalledWith(409);
      expect(res.json).toHaveBeenCalledWith({
        message: "Roadmap already exists in smart contract",
        success: false,
      });
    });
    // throw 500 error if something goes wrong
    it("should return 500 if an error occurs", async () => {
      req.body = {
        preId: "pre1",
        roadmapId: "roadmap1",
        roadmapName: "CleanCoast Mission",
        roadmapDescription: "Focused on plastic waste removal",
        prePkh: "b93e78824bcf5c34a62b2f573727b4bb8a1365ebd152bd6243ff8dc6",
        preSkh: "a786470d2a2c8bc00ecaf662a64407364be25325f33d1cb9446b4bd7",
        totalPlasticCredits: 100,
        totalPlasticTokens: 10000,
        totalPlastic: 100,
      };

      // Simulate database failure
      jest.spyOn(CompletedRoadmap, "findOne").mockImplementationOnce(() => {
        throw new Error("Database connection failed");
      });

      await roadmapController.initializeRoadmap(req, res);

      expect(res.status).toHaveBeenCalledWith(500);
      expect(res.json).toHaveBeenCalledWith({
        error: expect.any(Error),
        message: "Error occured during the creation of roadmap",
        success: false,
      });

      expect(console.error).toHaveBeenCalled();
    });

    // Return 200 if all goes well
    it("should return 200 if all goes well", async () => {
      req.body = {
        preId: "preNew",
        roadmapId: "roadmapNew",
        roadmapName: "Test",
        roadmapDescription: "Desc",
        prePkh: "b93e78824bcf5c34a62b2f573727b4bb8a1365ebd152bd6243ff8dc6",
        preSkh: "a786470d2a2c8bc00ecaf662a64407364be25325f33d1cb9446b4bd7",
        totalPlasticCredits: 10,
        totalPlasticTokens: 100,
        totalPlastic: 5,
      };
      jest.spyOn(CompletedRoadmap, "findOne").mockResolvedValueOnce(null);
      (
        fakeLucid.utxosAt as jest.Mock<() => Promise<UTxO[]>>
      ).mockResolvedValueOnce([]);

      // Mock transaction flow
      const mockSubmit = jest
        .fn<() => Promise<string>>()
        .mockResolvedValue("mockTxHash");
      const mockSignComplete = jest
        .fn()
        .mockImplementation(() => ({ submit: mockSubmit }));
      const mockSign = jest
        .fn()
        .mockReturnValue({ complete: mockSignComplete });
      const mockComplete = jest
        .fn<() => Promise<any>>()
        .mockResolvedValue({ sign: mockSign });
      (fakeLucid.newTx as jest.Mock).mockReturnValue({
        payToContract: jest.fn().mockReturnThis(),
        complete: mockComplete,
      });

      await roadmapController.initializeRoadmap(req, res);
      expect(res.status).toHaveBeenCalledWith(200);
      expect(res.json).toHaveBeenCalledWith({
        message: "Roadmap Initiated successfully",
        txHash: "mockTxHash",
        success: true,
      });
    });
  });

  describe("updateRoadmap controller", () => {
    // Should return 404 if no matching utxo found
    it("should return 404 if no matching UTXO is found", async () => {
      req.body = {
        preId: "missingPreId",
        roadmapId: "missingRoadmapId",
        soldPlasticCredit: 100,
      };

      // Mock no matching UTXO
      (
        fakeLucid.utxosAt as jest.Mock<() => Promise<UTxO[]>>
      ).mockResolvedValueOnce([]);

      await roadmapController.updateRoadmap(req, res);

      expect(res.status).toHaveBeenCalledWith(404);
      expect(res.json).toHaveBeenCalledWith({
        error: "Roadmap UTXO not found",
      });
    });

    // Should return 500 for unexpected error
    it("should handle unexpected errors and return 500", async () => {
      req.body = {
        preId: "preId",
        roadmapId: "roadmapId",
        soldPlasticCredit: 100,
      };

      // Force an error from lucid.utxosAt
      (fakeLucid.utxosAt as jest.Mock).mockImplementationOnce(() => {
        throw new Error("Unexpected error");
      });

      await roadmapController.updateRoadmap(req, res);

      expect(res.status).toHaveBeenCalledWith(500);
      expect(res.json).toHaveBeenCalledWith({
        message: "Something went wrong while updating Roadmap",
        success: false,
        error: expect.any(Error),
      });
      expect(console.error).toHaveBeenCalledWith(
        "Error in updateRoadmap:",
        expect.any(Error)
      );
    });

    // Should return 200 for successful update
    it("should return 200 and success response when roadmap is updated", async () => {
      req.body = {
        preId: "pre123",
        roadmapId: "road123",
        soldPlasticCredit: 100,
      };

      // Setup old datum with initial values
      const oldDatum = new Constr(0, [
        fromText("pre123"),
        fromText("road123"),
        fromText("MapName"),
        fromText("MapDesc"),
        0n, // initial progress
        fromText("adminPKH"),
        fromText("prePkh"),
        fromText("preSkh"),
        1000n, // totalPlasticCredits
        200n, // initial soldPlasticCredit
        50000n, // totalPlasticTokens
        10000n, // initial sentTokens
        100000n, // totalPlastic
        20000n, // initial recoveredPlastic
      ]);

      const matchedUtxo: UTxO = {
        txHash: "utxoHash",
        outputIndex: 0,
        address: "fakeRefiAddress",
        assets: {
          lovelace: 10000000n,
          [process.env.PLASTIK_TOKEN || ""]: 1000000n,
        },
        datum: Data.to(oldDatum),
      };

      // Mock UTXO retrieval to return matching UTXO
      (fakeLucid.utxosAt as jest.Mock).mockResolvedValue([
        matchedUtxo,
      ] as unknown as never);

      // Updated mock structure (consistent with initializeRoadmap test)
      const mockSubmit = jest
        .fn<() => Promise<string>>()
        .mockResolvedValue("mockTxHash");
      const mockSignComplete = jest
        .fn()
        .mockImplementation(() => ({ submit: mockSubmit }));
      const mockSign = jest
        .fn()
        .mockReturnValue({ complete: mockSignComplete });
      const mockComplete = jest
        .fn<() => Promise<any>>()
        .mockResolvedValue({ sign: mockSign });
      const mockTx = {
        collectFrom: jest.fn().mockReturnThis(),
        attachSpendingValidator: jest.fn().mockReturnThis(),
        addSigner: jest.fn().mockReturnThis(),
        payToContract: jest.fn().mockReturnThis(),
        complete: mockComplete,
      };
      (fakeLucid.newTx as jest.Mock).mockReturnValue(mockTx);

      await roadmapController.updateRoadmap(req, res);

      // Verify response
      expect(res.status).toHaveBeenCalledWith(200);
      expect(res.json).toHaveBeenCalledWith({
        message: "Roadmap updated successfully",
        txHash: "mockTxHash",
        newProgress: 30, // (200 + 100) / 1000 * 100 = 30%
        success: true,
      });

      // Verify transaction construction
      expect(mockTx.collectFrom).toHaveBeenCalledWith(
        [matchedUtxo],
        expect.anything()
      );
      expect(mockTx.attachSpendingValidator).toHaveBeenCalled();
      expect(mockTx.addSigner).toHaveBeenCalledWith("someAddress");
    });
  });

  describe("Get All Roadmaps Controller", () => {
    it("should handle UTxOs without datum", async () => {
      // Mock UTxO without datum
      (fakeLucid.utxosAt as jest.Mock).mockResolvedValue([
        {},
      ] as unknown as never);

      await roadmapController.getAllActiveRoadmaps(req, res);

      expect(res.json).toHaveBeenCalledWith(
        expect.objectContaining({
          roadmaps: [],
        })
      );
    });

    it("should return 500 when an error occurs", async () => {
      (fakeLucid.utxosAt as jest.Mock).mockRejectedValue(
        new Error("Database error") as unknown as never
      );

      await roadmapController.getAllActiveRoadmaps(req, res);

      expect(res.status).toHaveBeenCalledWith(500);
      expect(res.json).toHaveBeenCalledWith({
        message: "Something went wrong while fetching roadmaps",
        error: expect.any(Error),
        success: false,
      });
    });

    it("should return 200 and all roadmap details", async () => {
      // Construct mock datum
      const mockDatum = new Constr(0, [
        fromText("pre123"),
        fromText("road123"),
        fromText("MapName"),
        fromText("MapDesc"),
        0n, // initial progress
        fromText("adminPKH"),
        fromText("prePkh"),
        fromText("preSkh"),
        1000n, // totalPlasticCredits
        200n, // initial soldPlasticCredit
        50000n, // totalPlasticTokens
        10000n, // initial sentTokens
        100000n, // totalPlastic
        20000n, // initial recoveredPlastic
      ]);

      // Mock UTxOs
      const mockUtxoWithDatum = {
        txHash: "utxoHash",
        outputIndex: 0,
        address: "fakeRefiAddress",
        assets: {
          lovelace: 10000000n,
          e0b4a2454475355655a7449d3a064b38a22ba6fc83c637e2413ac172504c415354494b:
            1000000n,
        },
        datum: Data.to(mockDatum),
      };

      (fakeLucid.utxosAt as jest.Mock).mockResolvedValue([
        mockUtxoWithDatum,
      ] as unknown as never);

      await roadmapController.getAllActiveRoadmaps(req, res);

      expect(res.status).toHaveBeenCalledWith(200);
      expect(res.json).toHaveBeenCalledWith({
        message: "Fetched all roadmaps successfully",
        roadmaps: [
          {
            preId: "pre123",
            roadmapId: "road123",
            roadmapName: "MapName",
            roadmapDescription: "MapDesc",
            progress: 0,
            adminPkh: "61646d696e504b48",
            prePkh: "707265506b68",
            preSkh: "707265536b68",
            totalPlasticCredits: 1000,
            soldPlasticCredits: 200,
            totalPlasticTokens: 50000,
            sentPlasticTokens: 10000,
            totalPlastic: 100000,
            recoveredPlastic: 20000,
          },
        ],
        success: true,
      });
    });
  });

  describe("releaseFunds Controller", () => {
    // It Returns 404 when no Utxo Found
    it("returns 404 when no matching UTxO is found", async () => {
      (fakeLucid.utxosAt as jest.Mock).mockResolvedValue([
        {},
      ] as unknown as never);

      await roadmapController.saveRoadmap(req, res);
      expect(res.status).toHaveBeenCalledWith(404);
      expect(res.json).toHaveBeenCalledWith({
        error: "Roadmap UTXO not found",
        success: false,
      });
    });

    // It returns 500 for unexpected errors
    it("returns 500 on unexpected errors", async () => {
      (fakeLucid.utxosAt as jest.Mock).mockRejectedValue(
        new Error("Lucid Network Error") as unknown as never
      );
      await roadmapController.saveRoadmap(req, res);
      expect(res.status).toHaveBeenCalledWith(500);
      expect(res.json).toHaveBeenCalledWith({
        message: "Something went wrong during releasing funds",
        error: "Lucid Network Error",
        success: false,
      });
    });

    // It Return 201 for Successful release of funds
    it("returns 201 and txHash when a matching UTxO exists", async () => {
      // Mock environment variables
      process.env.PLASTIK_TOKEN = "PLASTIK_TOKEN";
      process.env.USDM = "USDM";
      process.env.DEAD_WALLET_ADDRESS = "dead_wallet";

      req.body = {
        preId: "pre123",
        roadmapId: "road123",
      };

      // Setup mock datum and UTxO
      const mockDatum = new Constr(0, [
        fromText("pre123"),
        fromText("road123"),
        fromText("MapName"),
        fromText("MapDesc"),
        10000n, // 100.00% progress
        "b93e78824bcf5c34a62b2f573727b4bb8a1365ebd152bd6243ff8dc6",
        "b93e78824bcf5c34a62b2f573727b4bb8a1365ebd152bd6243ff8dc6",
        "a786470d2a2c8bc00ecaf662a64407364be25325f33d1cb9446b4bd7",
        1000n, // totalPlasticCredits
        1000n, // soldPlasticCredits
        50000n, // totalPlasticTokens
        50000n, // sentPlasticTokens
        100000n, // totalPlastic
        100000n, // recoveredPlastic
      ]);

      const mockUtxo: UTxO = {
        txHash: "utxoHash",
        outputIndex: 0,
        address: "fakeRefiAddress",
        assets: {
          lovelace: 10000000n,
          [process.env.PLASTIK_TOKEN!]: 1000000n, // Match env asset ID
        },
        datum: Data.to(mockDatum),
      };

      (fakeLucid.utxosAt as jest.Mock).mockResolvedValue([
        mockUtxo,
      ] as unknown as never);

      const mockPayToAddress = jest.fn().mockReturnThis();
      // Mock transaction methods
      const mockSubmit = jest
        .fn<() => Promise<string>>()
        .mockResolvedValue("mockTxHash");
      const mockSignComplete = jest
        .fn()
        .mockImplementation(() => ({ submit: mockSubmit }));
      const mockSign = jest
        .fn()
        .mockReturnValue({ complete: mockSignComplete });
      const mockComplete = jest
        .fn<() => Promise<any>>()
        .mockResolvedValue({ sign: mockSign });
      const mockTx = {
        collectFrom: jest.fn().mockReturnThis(),
        attachSpendingValidator: jest.fn().mockReturnThis(),
        addSigner: jest.fn().mockReturnThis(),
        payToAddress: mockPayToAddress,
        complete: mockComplete,
      };
      (fakeLucid.newTx as jest.Mock).mockReturnValue(mockTx);

      // stub DB create
      jest.spyOn(CompletedRoadmap, "create").mockResolvedValue({} as any);

      await roadmapController.releaseFunds(req, res);

      // Assert response
      expect(res.status).toHaveBeenCalledWith(201);
      expect(res.json).toHaveBeenCalledWith({
        message: "Funds released successfully",
        success: true,
        txHash: "mockTxHash",
      });

      // ensure we saved the completedRoadmap
      expect(CompletedRoadmap.create).toHaveBeenCalledWith(
        expect.objectContaining({
          preId: "pre123",
          roadmapId: "road123",
        })
      );
    });
  });

  describe("queryTransaction Controller", () => {
    it("returns 404 when no matching UTxO is found", async () => {
      req.body = { txHash: "txHash", outputIndex: "outputIndex" };
      (fakeLucid.provider.getUtxosByOutRef as jest.Mock).mockResolvedValue(
        [] as unknown as never
      );

      await roadmapController.queryTransaction(req, res);
      expect(res.status).toHaveBeenCalledWith(404);
      expect(res.json).toHaveBeenCalledWith({
        message: "Utxo not found wait for sometime or check your txHash",
        success: false,
      });
    });

    // It returns 500 for unexpected errors
    it("returns 500 on unexpected errors", async () => {
      req.body = { txHash: "someTxHash", outputIndex: "0" };

      (fakeLucid.provider.getUtxosByOutRef as jest.Mock).mockImplementation(
        () => {
          throw new Error("Unexpected error");
        }
      );

      await roadmapController.queryTransaction(req, res);

      expect(res.status).toHaveBeenCalledWith(500);
      expect(res.json).toHaveBeenCalledWith(
        expect.objectContaining({
          success: false,
          message: expect.stringContaining("Something went wrong"),
        })
      );
    });

    it("should return 200 and decoded data when UTxO exists", async () => {
      req.body = { txHash: "txHash", outputIndex: "outputIndex" };
      const mockDatum = new Constr(0, [
        fromText("pre123"),
        fromText("road123"),
        fromText("MapName"),
        fromText("MapDesc"),
        10000n, // 100.00% progress
        "b93e78824bcf5c34a62b2f573727b4bb8a1365ebd152bd6243ff8dc6",
        "b93e78824bcf5c34a62b2f573727b4bb8a1365ebd152bd6243ff8dc6",
        "a786470d2a2c8bc00ecaf662a64407364be25325f33d1cb9446b4bd7",
        1000n, // totalPlasticCredits
        1000n, // soldPlasticCredits
        50000n, // totalPlasticTokens
        50000n, // sentPlasticTokens
        100000n, // totalPlastic
        100000n, // recoveredPlastic
      ]);

      const mockUtxo: UTxO = {
        txHash: "utxoHash",
        outputIndex: 0,
        address: "fakeRefiAddress",
        assets: {
          lovelace: 10000000n,
          [process.env.PLASTIK_TOKEN!]: 1000000n,
        },
        datum: Data.to(mockDatum),
      };

      // Mock UTxO retrieval to return matching UTxO
      (fakeLucid.provider.getUtxosByOutRef as jest.Mock).mockResolvedValue([
        mockUtxo,
      ] as unknown as never);
      const data = {
        assets: {
          PLASTIK_TOKEN: "1000000",
          lovelace: "10000000",
        },
        preId: "pre123",
        progress: 10000,
        recoveredPlastic: 100000,
        roadmapDescription: "MapDesc",
        roadmapId: "road123",
        roadmapName: "MapName",
        sentPlasticTokens: 50000,
        soldPlasticCredits: 1000,
        totalPlastic: 100000,
        totalPlasticCredits: 1000,
        totalPlasticTokens: 50000,
        txHash: "txHash",
      };
      await roadmapController.queryTransaction(req, res);
      expect(res.status).toHaveBeenCalledWith(200);
      expect(res.json).toHaveBeenCalledWith({
        message: "Utxo fetched succesfully",
        success: true,
        data,
      });
    });
  });

  describe("get All completed Roadmap Controller", () => {
    it("should return 200 and roadmaps when found", async () => {
      const mockRoadmaps = [
        {
          preId: "pre123",
          roadmapId: "roadmap123",
          roadmapName: "MapName",
          roadmapDescription: "MapDesc",
          progress: 0,
          adminPkh: "61646d696e504b48",
          prePkh: "707265506b68",
          preSkh: "707265536b68",
          totalPlasticCredits: 1000,
          soldPlasticCredits: 200,
          totalPlasticTokens: 50000,
          sentPlasticTokens: 10000,
          totalPlastic: 100000,
          recoveredPlastic: 20000,
        },
      ];

      jest
        .spyOn(CompletedRoadmap, "findAll")
        .mockResolvedValue(mockRoadmaps as unknown as never);

      await roadmapController.getAllCompletedRoadmaps(req, res);

      expect(res.status).toHaveBeenCalledWith(200);
      expect(res.json).toHaveBeenCalledWith({
        message: "Completed roadmaps retrieved successfully",
        roadmaps: mockRoadmaps,
      });
    });

    it("should return 200 and empty array when no roadmaps are found", async () => {
      const mockRoadmaps = [];

      jest
        .spyOn(CompletedRoadmap, "findAll")
        .mockResolvedValue(mockRoadmaps as unknown as never);

      await roadmapController.getAllCompletedRoadmaps(req, res);

      expect(res.status).toHaveBeenCalledWith(200);
      expect(res.json).toHaveBeenCalledWith({
        message: "No completed roadmaps found",
        roadmaps: mockRoadmaps,
      });
    });

    it("should return 500 and error message when an error occurs", async () => {
      jest
        .spyOn(CompletedRoadmap, "findAll")
        .mockRejectedValue(new Error("Database error"));

      await roadmapController.getAllCompletedRoadmaps(req, res);

      expect(res.status).toHaveBeenCalledWith(500);
      expect(res.json).toHaveBeenCalledWith({
        error: "Database error",
        message: "An error occurred while retrieving completed roadmaps",
      });
    });
  });
});
