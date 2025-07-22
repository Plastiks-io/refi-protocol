import { describe, it, expect, vi, beforeEach } from "vitest";
import { Worker } from "bullmq";
import { StakeContractJob } from "../../../src/bull/queues";
import { Cardano } from "../../../src/utils/cardano";
import { getIO } from "../../../src/utils/socket";

// Mock dependencies
vi.mock("../../../src/utils/cardano", () => {
  return {
    Cardano: vi.fn(),
  };
});

vi.mock("../../../src/utils/socket", () => ({
  getIO: vi.fn(),
}));

vi.mock("../../../src/bull/connection", () => ({
  connection: {},
}));

describe("updateStakeContract.worker", () => {
  let cardanoMockInstance: any;
  let ioMockInstance: any;
  let CardanoMock: any;
  let getIOMock: any;
  let consoleSpy: any;

  const jobData: StakeContractJob = {
    txHash: "tx123",
  };

  beforeEach(() => {
    vi.clearAllMocks();

    // Create mock instances
    cardanoMockInstance = {
      init: vi.fn(),
      updatedOnChain: vi.fn(),
    };

    ioMockInstance = {
      emit: vi.fn(),
    };

    // Setup mocks to return our instances
    CardanoMock = vi.mocked(Cardano);
    CardanoMock.mockImplementation(() => cardanoMockInstance);

    getIOMock = vi.mocked(getIO);
    getIOMock.mockReturnValue(ioMockInstance);

    // Mock console methods
    consoleSpy = {
      error: vi.spyOn(console, "error").mockImplementation(() => {}),
      log: vi.spyOn(console, "log").mockImplementation(() => {}),
    };
  });

  it("should process stake contract update successfully when transaction is updated", async () => {
    // Setup mocks for successful case where transaction is updated
    cardanoMockInstance.updatedOnChain.mockResolvedValue(true);

    const processor = async (job: any) => {
      const { txHash } = job.data;

      try {
        const cardano = new Cardano();
        await cardano.init();

        // 1) check if transaction has been updated on-chain
        const isUpdated = await cardano.updatedOnChain(txHash);

        // 2) emit it
        getIO().emit("stakeContractUpdated", isUpdated);

        return { success: true, isUpdated };
      } catch (error) {
        console.error("❌ Worker error:", error);
        throw error;
      }
    };

    const result = await processor({ data: jobData });

    expect(cardanoMockInstance.init).toHaveBeenCalledOnce();
    expect(cardanoMockInstance.updatedOnChain).toHaveBeenCalledWith("tx123");
    expect(ioMockInstance.emit).toHaveBeenCalledWith(
      "stakeContractUpdated",
      true
    );
    expect(result).toEqual({
      success: true,
      isUpdated: true,
    });
    expect(consoleSpy.error).not.toHaveBeenCalled();
  });

  it("should process stake contract update successfully when transaction is not updated", async () => {
    // Setup mocks for successful case where transaction is not updated
    cardanoMockInstance.updatedOnChain.mockResolvedValue(false);

    const processor = async (job: any) => {
      const { txHash } = job.data;

      try {
        const cardano = new Cardano();
        await cardano.init();

        // 1) check if transaction has been updated on-chain
        const isUpdated = await cardano.updatedOnChain(txHash);

        // 2) emit it
        getIO().emit("stakeContractUpdated", isUpdated);

        return { success: true, isUpdated };
      } catch (error) {
        console.error("❌ Worker error:", error);
        throw error;
      }
    };

    const result = await processor({ data: jobData });

    expect(cardanoMockInstance.init).toHaveBeenCalledOnce();
    expect(cardanoMockInstance.updatedOnChain).toHaveBeenCalledWith("tx123");
    expect(ioMockInstance.emit).toHaveBeenCalledWith(
      "stakeContractUpdated",
      false
    );
    expect(result).toEqual({
      success: true,
      isUpdated: false,
    });
    expect(consoleSpy.error).not.toHaveBeenCalled();
  });

  it("should handle error when cardano initialization fails", async () => {
    // Setup mocks for initialization failure
    cardanoMockInstance.init.mockRejectedValue(
      new Error("Initialization failed")
    );

    const processor = async (job: any) => {
      const { txHash } = job.data;

      try {
        const cardano = new Cardano();
        await cardano.init();

        // 1) check if transaction has been updated on-chain
        const isUpdated = await cardano.updatedOnChain(txHash);

        // 2) emit it
        getIO().emit("stakeContractUpdated", isUpdated);

        return { success: true, isUpdated };
      } catch (error) {
        console.error("❌ Worker error:", error);
        throw error;
      }
    };

    await expect(processor({ data: jobData })).rejects.toThrow(
      "Initialization failed"
    );

    expect(cardanoMockInstance.init).toHaveBeenCalledOnce();
    expect(cardanoMockInstance.updatedOnChain).not.toHaveBeenCalled();
    expect(ioMockInstance.emit).not.toHaveBeenCalled();
    expect(consoleSpy.error).toHaveBeenCalledWith(
      "❌ Worker error:",
      expect.any(Error)
    );
  });

  it("should handle error when updatedOnChain fails", async () => {
    // Setup mocks for updatedOnChain failure
    cardanoMockInstance.updatedOnChain.mockRejectedValue(
      new Error("Failed to check transaction status")
    );

    const processor = async (job: any) => {
      const { txHash } = job.data;

      try {
        const cardano = new Cardano();
        await cardano.init();

        // 1) check if transaction has been updated on-chain
        const isUpdated = await cardano.updatedOnChain(txHash);

        // 2) emit it
        getIO().emit("stakeContractUpdated", isUpdated);

        return { success: true, isUpdated };
      } catch (error) {
        console.error("❌ Worker error:", error);
        throw error;
      }
    };

    await expect(processor({ data: jobData })).rejects.toThrow(
      "Failed to check transaction status"
    );

    expect(cardanoMockInstance.init).toHaveBeenCalledOnce();
    expect(cardanoMockInstance.updatedOnChain).toHaveBeenCalledWith("tx123");
    expect(ioMockInstance.emit).not.toHaveBeenCalled();
    expect(consoleSpy.error).toHaveBeenCalledWith(
      "❌ Worker error:",
      expect.any(Error)
    );
  });

  it("should process with different transaction hash", async () => {
    const differentJobData: StakeContractJob = {
      txHash: "tx999",
    };

    // Setup mocks for successful case with different txHash
    cardanoMockInstance.updatedOnChain.mockResolvedValue(true);

    const processor = async (job: any) => {
      const { txHash } = job.data;

      try {
        const cardano = new Cardano();
        await cardano.init();

        // 1) check if transaction has been updated on-chain
        const isUpdated = await cardano.updatedOnChain(txHash);

        // 2) emit it
        getIO().emit("stakeContractUpdated", isUpdated);

        return { success: true, isUpdated };
      } catch (error) {
        console.error("❌ Worker error:", error);
        throw error;
      }
    };

    const result = await processor({ data: differentJobData });

    expect(cardanoMockInstance.updatedOnChain).toHaveBeenCalledWith("tx999");
    expect(ioMockInstance.emit).toHaveBeenCalledWith(
      "stakeContractUpdated",
      true
    );
    expect(result).toEqual({
      success: true,
      isUpdated: true,
    });
  });

  it("should handle null or undefined updatedOnChain result", async () => {
    // Setup mocks for null/undefined result
    cardanoMockInstance.updatedOnChain.mockResolvedValue(null);

    const processor = async (job: any) => {
      const { txHash } = job.data;

      try {
        const cardano = new Cardano();
        await cardano.init();

        // 1) check if transaction has been updated on-chain
        const isUpdated = await cardano.updatedOnChain(txHash);

        // 2) emit it
        getIO().emit("stakeContractUpdated", isUpdated);

        return { success: true, isUpdated };
      } catch (error) {
        console.error("❌ Worker error:", error);
        throw error;
      }
    };

    const result = await processor({ data: jobData });

    expect(cardanoMockInstance.init).toHaveBeenCalledOnce();
    expect(cardanoMockInstance.updatedOnChain).toHaveBeenCalledWith("tx123");
    expect(ioMockInstance.emit).toHaveBeenCalledWith(
      "stakeContractUpdated",
      null
    );
    expect(result).toEqual({
      success: true,
      isUpdated: null,
    });
  });
});
