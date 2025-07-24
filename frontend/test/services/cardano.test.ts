import { describe, it, expect, vi, beforeEach } from "vitest";
import { Cardano } from "../../src/services/cardano";
import { Blockfrost, Constr, Lucid } from "lucid-cardano";

// ----------------------- MOCKS -----------------------

vi.mock("lucid-cardano", async (importOriginal) => {
  const actual = await importOriginal();
  const lucidCardanoModule = actual as typeof import("lucid-cardano");
  return {
    ...(lucidCardanoModule as object),
    Constr: lucidCardanoModule.Constr,
    Data: {
      from: vi.fn((x) => x),
      to: vi.fn((x) => x),
    },
    Lucid: {
      new: vi.fn(async () => ({
        wallet: {
          address: vi.fn(async () => "mock-wallet-address"),
          getUtxos: vi.fn(async () => []),
          selectWallet: vi.fn(),
        },
        newTx: vi.fn(() => ({
          payToContract: vi.fn().mockReturnThis(),
          collectFrom: vi.fn().mockReturnThis(),
          attachSpendingValidator: vi.fn().mockReturnThis(),
          attachWithdrawalValidator: vi.fn().mockReturnThis(),
          payToAddress: vi.fn().mockReturnThis(),
          addSigner: vi.fn().mockReturnThis(),
          complete: vi.fn(async () => ({
            sign: vi.fn().mockReturnThis(),
            submit: vi.fn(async () => "mock-tx-hash"),
            complete: vi.fn().mockReturnThis(),
          })),
        })),
        selectWallet: vi.fn(),
        utils: {
          validatorToAddress: vi.fn(() => "mock-address"),
          keyHashToCredential: vi.fn((hash) => ({ hash })),
          credentialToAddress: vi.fn(() => "mock-pre-address"),
        },
        utxosAt: vi.fn(async () => []),
      })),
    },
    Blockfrost: class MockBlockfrost {
      constructor(public url: string, public key: string) {}
    },
    getAddressDetails: vi.fn(() => ({
      paymentCredential: { hash: "paymentHash" },
      stakeCredential: { hash: "stakeHash" },
    })),

    toText: vi.fn((hex: string) => {
      try {
        return Buffer.from(hex, "hex").toString("utf8");
      } catch {
        return "[invalid hex]";
      }
    }),

    fromText: vi.fn((str: string) => Buffer.from(str, "utf8").toString("hex")),
  };
});

vi.mock("axios", () => ({
  default: {
    post: vi.fn(async () => ({ data: { message: "success" } })),
  },
}));

vi.mock("sonner", () => ({
  toast: {
    error: vi.fn(),
    success: vi.fn(),
  },
}));

// ----------------------- TEST CASES -----------------------

describe("Cardano (mocked)", () => {
  let cardano: Cardano;

  beforeEach(() => {
    cardano = new Cardano();
  });

  describe("buildLenderDatum()", () => {
    it("should build correct Constr from LenderDatum", () => {
      const input = {
        adminsPkh: ["admin1"],
        totalPT: 1000n,
        totalReward: 500n,
        lenders: [
          ["user1", [300n, 100n]] as [string, [bigint, bigint]],
          ["user2", [700n, 400n]] as [string, [bigint, bigint]],
        ],
      };
      const result = cardano.buildLenderDatum(input);
      expect(result.index).toBe(0);
      expect(result.fields[0]).toEqual(["admin1"]);
    });
  });

  describe("parseLenderDatum()", () => {
    it("should parse Constr to LenderDatum", () => {
      const input = new Constr(0, [
        ["admin1"],
        1000n,
        500n,
        [
          new Constr(0, ["user1", new Constr(0, [300n, 100n])]),
          new Constr(0, ["user2", new Constr(0, [700n, 400n])]),
        ],
      ]);
      const result = cardano.parseLenderDatum(input);
      expect(result.totalPT).toBe(1000n);
      expect(result.lenders).toHaveLength(2);
    });

    it("should throw if Constr index is not 0", () => {
      const invalid = new Constr(1, []);
      expect(() => cardano.parseLenderDatum(invalid)).toThrow();
    });
  });

  describe("buildLenderAction()", () => {
    it("should build Deposit Constr", () => {
      const action = cardano.buildLenderAction({ type: "Deposit" });
      expect(action.index).toBe(0);
      expect(action.fields).toEqual([]);
    });

    it("should throw if Withdraw is missing amount", () => {
      expect(() => cardano.buildLenderAction({ type: "Withdraw" })).toThrow();
    });

    it("should build Redeem Constr", () => {
      const action = cardano.buildLenderAction({ type: "Redeem" });
      expect(action.index).toBe(2);
    });

    it("should throw on unknown action type", () => {
      // @ts-expect-error
      expect(() => cardano.buildLenderAction({ type: "Unknown" })).toThrow();
    });
  });

  describe("parseRoadmapDatum()", () => {
    it("should parse roadmap datum correctly", () => {
      const input = new Constr(0, [
        "706d01", // preId
        "726d01", // roadmapId
        "4e616d65", // Name
        "44657363", // Desc
        10n,
        ["admin1"],
        "prePkh",
        "preSkh",
        1000n,
        500n,
        800n,
        400n,
        1200n,
        200n,
        "323032332d3031", // 2023-01
      ]);
      const result = cardano.parseRoadmapDatum(input);
      expect(result.roadmapName).toBe("Name");
      expect(result.roadmapDescription).toBe("Desc");
      expect(result.recoverPlastic).toBe(200n);
      expect(result.createdAt).toBe("2023-01");
    });

    it("should throw on invalid roadmap Constr", () => {
      const input = new Constr(1, []);
      expect(() => cardano.parseRoadmapDatum(input)).toThrow();
    });
  });

  describe("getLucid()", () => {
    it("should throw if lucid is not initialized", () => {
      expect(() => cardano.getLucid()).toThrow(/Lucid not initialized/);
    });
  });

  describe("getPubKeyHash()", () => {
    it("should return paymentCredential hash", async () => {
      const lucid: any = await (
        await import("lucid-cardano")
      ).Lucid.new(new Blockfrost("url", "projectId"), "Preprod");
      const result = await cardano.getPubKeyHash(lucid);
      expect(result).toBe("paymentHash");
    });
  });

  describe("init()", () => {
    it("should throw on unsupported network ID", async () => {
      await expect(() => cardano.init(99)).rejects.toThrow(
        /Unsupported network ID/
      );
    });
  });
});
