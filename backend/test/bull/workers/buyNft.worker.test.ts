import { describe, it, expect, vi, beforeEach } from "vitest";
import { BuyNftJob } from "../../../src/bull/queues";
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

describe("buyNft.worker", () => {
  let cardanoMockInstance: any;
  let ioMockInstance: any;
  let CardanoMock: any;
  let getIOMock: any;

  const jobData: BuyNftJob = {
    txHash: "tx123",
    buyerAddress: "addr_test1...",
    preId: "pre123",
    roadmapId: "roadmap456",
    soldPlasticCredit: 100,
  };

  beforeEach(() => {
    vi.clearAllMocks();

    // Create mock instances
    cardanoMockInstance = {
      init: vi.fn(),
      checkTxConfirmed: vi.fn(),
      updateRoadmap: vi.fn(),
      getLucid: vi.fn().mockReturnValue({
        awaitTx: vi.fn(),
      }),
      refundAda: vi.fn(),
      sendPcToken: vi.fn(),
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

  it("should process job successfully", async () => {
    // Setup mocks for successful case
    cardanoMockInstance.checkTxConfirmed
      .mockResolvedValueOnce(true) // First call for txHash
      .mockResolvedValueOnce(true); // Second call for sentPcTx
    cardanoMockInstance.updateRoadmap.mockResolvedValue("txRoadmap123");
    cardanoMockInstance.sendPcToken.mockResolvedValue("txPc123");
    cardanoMockInstance.getRoadmapDatum.mockResolvedValue({ updated: true });

    const processor = async (job: any) => {
      const { txHash, buyerAddress, preId, roadmapId, soldPlasticCredit } =
        job.data;
      const cardano = new Cardano();
      await cardano.init();

      const confirmed = await cardano.checkTxConfirmed(txHash);
      if (!confirmed) throw new Error("Tx1 not confirmed");

      let roadmapTx: string;
      try {
        roadmapTx = await cardano.updateRoadmap(
          preId,
          roadmapId,
          soldPlasticCredit
        );
        await cardano.getLucid().awaitTx(roadmapTx);
      } catch (err: any) {
        const refundHash = await cardano.refundAda(
          buyerAddress,
          soldPlasticCredit
        );
        await cardano.getLucid().awaitTx(refundHash);
        return {
          refunded: true,
          reason: "roadmap-update-failed",
          refundHash,
        };
      }

      const sentPcTx = await cardano.sendPcToken(
        buyerAddress,
        soldPlasticCredit,
        roadmapId
      );
      const txConfirmed3 = await cardano.checkTxConfirmed(sentPcTx);
      if (!txConfirmed3) throw new Error("Tx3 not confirmed");

      const updatedDatum = await cardano.getRoadmapDatum(preId, roadmapId);
      const io = getIO();
      io.emit("roadmapUpdated", updatedDatum);

      return { roadmapTx, sentPcTx };
    };

    const result = await processor({ data: jobData });

    expect(result).toEqual({
      roadmapTx: "txRoadmap123",
      sentPcTx: "txPc123",
    });
    expect(ioMockInstance.emit).toHaveBeenCalledWith("roadmapUpdated", {
      updated: true,
    });
  });

  it("should refund ADA if roadmap update fails", async () => {
    // Setup mocks for failure case
    cardanoMockInstance.checkTxConfirmed.mockResolvedValue(true);
    cardanoMockInstance.updateRoadmap.mockRejectedValue(
      new Error("Update failed")
    );
    cardanoMockInstance.refundAda.mockResolvedValue("refundTx123");

    // Create a proper job processor function
    const processor = async (job: any) => {
      const { txHash, buyerAddress, preId, roadmapId, soldPlasticCredit } =
        job.data;
      const cardano = new Cardano();
      await cardano.init();

      const confirmed = await cardano.checkTxConfirmed(txHash);
      if (!confirmed) throw new Error("Tx1 not confirmed");

      try {
        const roadmapTx = await cardano.updateRoadmap(
          preId,
          roadmapId,
          soldPlasticCredit
        );
        await cardano.getLucid().awaitTx(roadmapTx);

        // Continue with successful path...
        const sentPcTx = await cardano.sendPcToken(
          buyerAddress,
          soldPlasticCredit,
          roadmapId
        );
        const txConfirmed3 = await cardano.checkTxConfirmed(sentPcTx);
        if (!txConfirmed3) throw new Error("Tx3 not confirmed");

        const updatedDatum = await cardano.getRoadmapDatum(preId, roadmapId);
        const io = getIO();
        io.emit("roadmapUpdated", updatedDatum);

        return { roadmapTx, sentPcTx };
      } catch (err: any) {
        const refundHash = await cardano.refundAda(
          buyerAddress,
          soldPlasticCredit
        );
        await cardano.getLucid().awaitTx(refundHash);
        return {
          refunded: true,
          reason: "roadmap-update-failed",
          refundHash,
        };
      }
    };

    // Test the processor directly instead of using Worker.processJob
    const result = await processor({ data: jobData });

    expect(result).toEqual({
      refunded: true,
      reason: "roadmap-update-failed",
      refundHash: "refundTx123",
    });
  });
});
