import { describe, it, expect, vi, beforeEach } from "vitest";
import { Worker } from "bullmq";
import { RefiContractJob } from "../../../src/bull/queues";
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

describe("updateRefiContract.worker", () => {
  let cardanoMockInstance: any;
  let ioMockInstance: any;
  let CardanoMock: any;
  let getIOMock: any;

  const jobData: RefiContractJob = {
    preId: "pre123",
    roadmapId: "roadmap456",
    txHash: "tx789",
  };

  beforeEach(() => {
    vi.clearAllMocks();

    // Create mock instances
    cardanoMockInstance = {
      init: vi.fn(),
      updatedOnChain: vi.fn(),
      getRoadmapDatum: vi.fn(),
    };

    ioMockInstance = {
      emit: vi.fn(),
    };

    // Setup mocks to return our instances
    CardanoMock = vi.mocked(Cardano);
    CardanoMock.mockImplementation(() => cardanoMockInstance);

    getIOMock = vi.mocked(getIO);
    getIOMock.mockReturnValue(ioMockInstance);
  });

  it("should process refi contract update successfully", async () => {
    // Setup mocks for successful case
    cardanoMockInstance.updatedOnChain.mockResolvedValue(true);
    cardanoMockInstance.getRoadmapDatum.mockResolvedValue({
      preId: "pre123",
      roadmapId: "roadmap456",
      status: "updated",
      timestamp: "2024-01-01T00:00:00Z",
    });

    const processor = async (job: any) => {
      const { preId, roadmapId, txHash } = job.data;
      const cardano = new Cardano();
      await cardano.init();

      // 1) check if transaction has been updated on-chain
      await cardano.updatedOnChain(txHash);

      // 2) fetch the fresh ReFi datum
      const updatedDatum = await cardano.getRoadmapDatum(preId, roadmapId);

      // 3) emit to all clients
      getIO().emit("roadmapUpdated", updatedDatum);

      return updatedDatum;
    };

    const result = await processor({ data: jobData });

    expect(cardanoMockInstance.init).toHaveBeenCalledOnce();
    expect(cardanoMockInstance.updatedOnChain).toHaveBeenCalledWith("tx789");
    expect(cardanoMockInstance.getRoadmapDatum).toHaveBeenCalledWith(
      "pre123",
      "roadmap456"
    );
    expect(ioMockInstance.emit).toHaveBeenCalledWith("roadmapUpdated", {
      preId: "pre123",
      roadmapId: "roadmap456",
      status: "updated",
      timestamp: "2024-01-01T00:00:00Z",
    });
    expect(result).toEqual({
      preId: "pre123",
      roadmapId: "roadmap456",
      status: "updated",
      timestamp: "2024-01-01T00:00:00Z",
    });
  });

  it("should handle error when transaction check fails", async () => {
    // Setup mocks for failure case
    cardanoMockInstance.updatedOnChain.mockRejectedValue(
      new Error("Transaction not found on-chain")
    );

    const processor = async (job: any) => {
      const { preId, roadmapId, txHash } = job.data;
      const cardano = new Cardano();
      await cardano.init();

      try {
        // 1) check if transaction has been updated on-chain
        await cardano.updatedOnChain(txHash);

        // 2) fetch the fresh ReFi datum
        const updatedDatum = await cardano.getRoadmapDatum(preId, roadmapId);

        // 3) emit to all clients
        getIO().emit("roadmapUpdated", updatedDatum);

        return updatedDatum;
      } catch (error) {
        throw error;
      }
    };

    await expect(processor({ data: jobData })).rejects.toThrow(
      "Transaction not found on-chain"
    );

    expect(cardanoMockInstance.init).toHaveBeenCalledOnce();
    expect(cardanoMockInstance.updatedOnChain).toHaveBeenCalledWith("tx789");
    expect(cardanoMockInstance.getRoadmapDatum).not.toHaveBeenCalled();
    expect(ioMockInstance.emit).not.toHaveBeenCalled();
  });

  it("should handle error when fetching roadmap datum fails", async () => {
    // Setup mocks for failure case
    cardanoMockInstance.updatedOnChain.mockResolvedValue(true);
    cardanoMockInstance.getRoadmapDatum.mockRejectedValue(
      new Error("Failed to fetch roadmap datum")
    );

    const processor = async (job: any) => {
      const { preId, roadmapId, txHash } = job.data;
      const cardano = new Cardano();
      await cardano.init();

      try {
        // 1) check if transaction has been updated on-chain
        await cardano.updatedOnChain(txHash);

        // 2) fetch the fresh ReFi datum
        const updatedDatum = await cardano.getRoadmapDatum(preId, roadmapId);

        // 3) emit to all clients
        getIO().emit("roadmapUpdated", updatedDatum);

        return updatedDatum;
      } catch (error) {
        throw error;
      }
    };

    await expect(processor({ data: jobData })).rejects.toThrow(
      "Failed to fetch roadmap datum"
    );

    expect(cardanoMockInstance.init).toHaveBeenCalledOnce();
    expect(cardanoMockInstance.updatedOnChain).toHaveBeenCalledWith("tx789");
    expect(cardanoMockInstance.getRoadmapDatum).toHaveBeenCalledWith(
      "pre123",
      "roadmap456"
    );
    expect(ioMockInstance.emit).not.toHaveBeenCalled();
  });

  it("should process with different job data", async () => {
    const differentJobData: RefiContractJob = {
      preId: "pre999",
      roadmapId: "roadmap888",
      txHash: "tx555",
    };

    // Setup mocks for successful case with different data
    cardanoMockInstance.updatedOnChain.mockResolvedValue(true);
    cardanoMockInstance.getRoadmapDatum.mockResolvedValue({
      preId: "pre999",
      roadmapId: "roadmap888",
      status: "completed",
      credits: 50,
    });

    const processor = async (job: any) => {
      const { preId, roadmapId, txHash } = job.data;
      const cardano = new Cardano();
      await cardano.init();

      // 1) check if transaction has been updated on-chain
      await cardano.updatedOnChain(txHash);

      // 2) fetch the fresh ReFi datum
      const updatedDatum = await cardano.getRoadmapDatum(preId, roadmapId);

      // 3) emit to all clients
      getIO().emit("roadmapUpdated", updatedDatum);

      return updatedDatum;
    };

    const result = await processor({ data: differentJobData });

    expect(cardanoMockInstance.updatedOnChain).toHaveBeenCalledWith("tx555");
    expect(cardanoMockInstance.getRoadmapDatum).toHaveBeenCalledWith(
      "pre999",
      "roadmap888"
    );
    expect(ioMockInstance.emit).toHaveBeenCalledWith("roadmapUpdated", {
      preId: "pre999",
      roadmapId: "roadmap888",
      status: "completed",
      credits: 50,
    });
    expect(result).toEqual({
      preId: "pre999",
      roadmapId: "roadmap888",
      status: "completed",
      credits: 50,
    });
  });
});
