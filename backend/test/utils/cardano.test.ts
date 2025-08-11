import { describe, it, expect, beforeEach, vi } from "vitest";
import { Cardano } from "../../src/utils/cardano";
import {
  Lucid,
  Blockfrost,
  Address,
  Credential,
  Datum,
  DatumHash,
  Delegation,
  OutRef,
  ProtocolParameters,
  RewardAddress,
  Transaction,
  TxHash,
  Unit,
  UTxO,
} from "lucid-cardano";

vi.mock("lucid-cardano", () => {
  return {
    Lucid: {
      new: vi.fn().mockResolvedValue({
        utils: {
          validatorToAddress: vi.fn().mockReturnValue("addr1_fake_address"),
          keyHashToCredential: vi.fn().mockReturnValue("cred"),
          credentialToAddress: vi.fn().mockReturnValue("pre_addr"),
        },
        awaitTx: vi.fn().mockResolvedValue(true),
        newTx: vi.fn().mockReturnThis(),
        selectWalletFromSeed: vi.fn(),
        wallet: {
          address: vi.fn().mockResolvedValue("admin_addr"),
        },
        utxosAt: vi.fn().mockResolvedValue([]),
      }),
    },
    Blockfrost: vi.fn(),
    Data: {
      from: vi.fn(),
      to: vi.fn(),
    },
    Constr: vi.fn(),
    toText: vi.fn((val: string) => val),
    fromText: vi.fn((val: string) => val),
  };
});

describe("Cardano class - constructor & initialization", () => {
  beforeEach(() => {
    process.env.PLASTIC_TOKEN = "pt_token";
    process.env.USDM_TOKEN = "usdm_token";
    process.env.PC_ASSET_ID = "pc_token";
    process.env.STAKE_REWARD_CBOR = "stake_cbor";
    process.env.REFI_CBOR = "refi_cbor";
    process.env.BLOCKFROST_URL = "https://test.blockfrost.io";
    process.env.BLOCKFROST_PROJECT_ID = "test_project_id";
  });

  it("should instantiate with correct validators and assets", () => {
    const cardano = new Cardano();
    expect(cardano.refiValidator.script).toBe("refi_cbor");
    expect(cardano.stakeRewardValidator.type).toBe("PlutusV2");
  });

  it("should initialize Lucid instance", async () => {
    const cardano = new Cardano();
    await cardano.init();

    expect(cardano.lucidInstance).toBeDefined();
  });

  it("should throw if getLucid is called before init", () => {
    const cardano = new Cardano();
    expect(() => cardano.getLucid()).toThrow(
      "Lucid not initialized: call init(networkId) first."
    );
  });
});

describe("Cardano class - stakeRewardAddress", () => {
  it("should throw if Lucid is not initialized", () => {
    const cardano = new Cardano();
    expect(() => cardano.stakeRewardAddress).toThrow("Lucid not initialized");
  });

  it("should return stake reward address after init", async () => {
    const cardano = new Cardano();
    await cardano.init();
    const addr = cardano.stakeRewardAddress;
    expect(addr).toBe("addr1_fake_address");
  });
});

describe("Cardano class - checkTxConfirmed", () => {
  it("should return true if tx is confirmed", async () => {
    const cardano = new Cardano();
    await cardano.init();
    const confirmed = await cardano.checkTxConfirmed("tx_hash");
    expect(confirmed).toBe(true);
  });

  it("should return false if tx confirmation fails", async () => {
    const lucid = await Lucid.new(
      {
        getProtocolParameters: function (): Promise<ProtocolParameters> {
          throw new Error("Function not implemented.");
        },
        getUtxos: function (
          addressOrCredential: Address | Credential
        ): Promise<UTxO[]> {
          throw new Error("Function not implemented.");
        },
        getUtxosWithUnit: function (
          addressOrCredential: Address | Credential,
          unit: Unit
        ): Promise<UTxO[]> {
          throw new Error("Function not implemented.");
        },
        getUtxoByUnit: function (unit: Unit): Promise<UTxO> {
          throw new Error("Function not implemented.");
        },
        getUtxosByOutRef: function (outRefs: Array<OutRef>): Promise<UTxO[]> {
          throw new Error("Function not implemented.");
        },
        getDelegation: function (
          rewardAddress: RewardAddress
        ): Promise<Delegation> {
          throw new Error("Function not implemented.");
        },
        getDatum: function (datumHash: DatumHash): Promise<Datum> {
          throw new Error("Function not implemented.");
        },
        awaitTx: function (
          txHash: TxHash,
          checkInterval?: number
        ): Promise<boolean> {
          throw new Error("Function not implemented.");
        },
        submitTx: function (tx: Transaction): Promise<TxHash> {
          throw new Error("Function not implemented.");
        },
      },
      "Preprod"
    );
    (lucid.awaitTx as any).mockRejectedValueOnce(new Error("tx not found"));

    const cardano = new Cardano();
    await cardano.init();
    const result = await cardano.checkTxConfirmed("fail_tx");
    expect(result).toBe(false);
  });
});

describe("Cardano class - updatedOnChain", () => {
  beforeEach(() => {
    global.fetch = vi.fn();
    process.env.BLOCKFROST_PROJECT_ID = "test";
    process.env.BLOCKFROST_URL = "https://blockfrost.io";
  });

  it("should return true if tx is confirmed via polling", async () => {
    (global.fetch as any).mockResolvedValueOnce({ ok: true });

    const cardano = new Cardano();
    const confirmed = await cardano.updatedOnChain("tx123");
    expect(confirmed).toBe(true);
  });
});
