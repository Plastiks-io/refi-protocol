import { describe, it, expect, beforeEach, vi, afterEach } from "vitest";

describe("Validator Constants", () => {
  // Store original env values to restore later
  const originalEnv = { ...process.env };

  beforeEach(() => {
    // Clear all modules from cache
    vi.resetModules();

    // Set up fresh environment
    process.env.STAKE_REWARD_CBOR = "stake-reward-cbor-script";
    process.env.REFI_CBOR = "refi-cbor-script";
  });

  afterEach(() => {
    // Restore original environment
    process.env = { ...originalEnv };
    vi.resetModules();
  });

  it("should correctly export stakeRewardValidator from env", async () => {
    const { stakeRewardValidator } = await import(
      "../../src/contract/contracts"
    );
    expect(stakeRewardValidator).toBeDefined();
    expect(stakeRewardValidator.type).toBe("PlutusV2");
    expect(stakeRewardValidator.script).toBe("stake-reward-cbor-script");
  });

  it("should correctly export refiValidator from env", async () => {
    const { refiValidator } = await import("../../src/contract/contracts");
    expect(refiValidator).toBeDefined();
    expect(refiValidator.type).toBe("PlutusV2");
    expect(refiValidator.script).toBe("refi-cbor-script");
  });

  it("should fail if STAKE_REWARD_CBOR is missing", async () => {
    // Remove the specific env var
    delete process.env.STAKE_REWARD_CBOR;

    // Clear module cache to force re-evaluation
    vi.resetModules();

    // Import should throw an error
    await expect(import("../../src/contract/contracts")).rejects.toThrow(
      /Missing environment variable: STAKE_REWARD_CBOR/
    );
  });

  it("should fail if REFI_CBOR is missing", async () => {
    // Remove the specific env var
    delete process.env.REFI_CBOR;

    // Clear module cache to force re-evaluation
    vi.resetModules();

    await expect(import("../../src/contract/contracts")).rejects.toThrow(
      /Missing environment variable: REFI_CBOR/
    );
  });
});
