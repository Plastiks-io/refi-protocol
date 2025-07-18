// src/bull.ts
import { Queue, Worker, Job } from "bullmq";
import { Redis } from "ioredis";
import { Cardano } from "./utils/cardano.js";

export interface BuyNftJob {
  txHash: string;
  buyerAddress: string;
  preId: string;
  roadmapId: string;
  soldPlasticCredit: number;
}

const connection = new Redis({
  host: "127.0.0.1",
  port: 6379,
  maxRetriesPerRequest: null,
  enableReadyCheck: false,
});

export const buyNftQueue = new Queue<BuyNftJob>("buyNftQueue", {
  connection,
});

new Worker<BuyNftJob>(
  "buyNftQueue",
  async (job) => {
    const { txHash, buyerAddress, preId, roadmapId, soldPlasticCredit } =
      job.data;
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
    const txConfirmed3 = await cardano.checkTxConfirmed(sentPcTx);
    if (!txConfirmed3) throw new Error("Tx3 not confirmed");

    return { roadmapTx, sentPcTx };
  },
  { connection, concurrency: 1 }
);
