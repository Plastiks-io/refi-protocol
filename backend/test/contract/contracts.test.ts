import { describe, it, expect, vi } from "vitest";

// Mock the config module
vi.mock("../../src/config/environment", () => {
  return {
    default: {
      CONTRACTS: {
        STAKE_REWARD_CBOR: "4e4d01000033222220051200120011",
        REFI_CBOR: "4e4d010000998877665544332211",
      },
      // Add other config properties if needed for these tests
    },
  };
});

describe("Lucid Validator exports", () => {
  it("should export stakeRewardValidator with type and script", async () => {
    // Dynamically import to ensure fresh module with mock
    const { stakeRewardValidator } = await import(
      "../../src/contract/contracts"
    );

    expect(stakeRewardValidator).toBeDefined();
    expect(stakeRewardValidator.type).toBe("PlutusV2");
    expect(stakeRewardValidator.script).toBe("4e4d01000033222220051200120011");
  });

  it("should export refiValidator with type and script", async () => {
    // Dynamically import to ensure fresh module with mock
    const { refiValidator } = await import("../../src/contract/contracts");

    expect(refiValidator).toBeDefined();
    expect(refiValidator.type).toBe("PlutusV2");
    expect(refiValidator.script).toBe("4e4d010000998877665544332211");
  });
});
