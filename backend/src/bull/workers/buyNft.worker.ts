import { Worker } from "bullmq";
import { Cardano } from "../../utils/cardano.js";
import { getIO } from "../../utils/socket.js";
import { BuyNftJob } from "../queues.js";
import { connection } from "../connection.js";

const worker = new Worker<BuyNftJob>(
  "buyNftQueue",
  async (job) => {
    const { txHash, buyerAddress, preId, roadmapId, soldPlasticCredit } =
      job.data;

    console.log("👷 Buy NFT worker processing job", txHash);

    const cardano = new Cardano();
    await cardano.init();
    // 1. Confirm Tx1 (ADA received)
    const confirmed = await cardano.checkTxConfirmed(txHash);
    if (!confirmed) throw new Error("Tx1 not confirmed");

    // 2. Update Roadmap (Tx2)
    let roadmapTx: string;
    try {
      roadmapTx = await cardano.updateRoadmap(
        preId,
        roadmapId,
        soldPlasticCredit
      );
      await cardano.getLucid().awaitTx(roadmapTx);
    } catch (err: any) {
      console.warn("⚠️ Roadmap update failed, refunding ADA:", err);
      const refundHash = await cardano.refundAda(
        buyerAddress,
        soldPlasticCredit
      );
      await cardano.getLucid().awaitTx(refundHash);
      return { refunded: true, reason: "roadmap-update-failed", refundHash };
    }
    // 3. Tx3: Send PC when all goes well
    const sentPcTx = await cardano.sendPcToken(
      buyerAddress,
      soldPlasticCredit,
      roadmapId
    );
    // 4) check if transaction has been updated on-chain for updated Roadmap
    await cardano.updatedOnChain(roadmapTx);

    // 5) fetch the fresh ReFi datum
    const updatedDatum = await cardano.getRoadmapDatum(preId, roadmapId);

    // **EMIT THE SOCKET EVENT**
    const io = getIO();
    io.emit("roadmapUpdated", updatedDatum);

    return { roadmapTx, sentPcTx, updatedDatum };
  },
  { connection, concurrency: 1 }
);

// Add event listeners for debugging
worker.on("ready", () => {
  console.log("👷 Buy NFT worker is ready");
});

worker.on("error", (err) => {
  console.error("❌ Buy NFT worker error:", err);
});

worker.on("failed", (job, err) => {
  console.error(`❌ Job ${job?.id} failed:`, err);
});

worker.on("completed", (job, result) => {
  console.log(`✅ Job ${job.id} completed:`, result);
});
