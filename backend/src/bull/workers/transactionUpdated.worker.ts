// updateStakeContract.worker.ts
import { Worker } from "bullmq";
import { Cardano } from "../../utils/cardano.js";
import { getIO } from "../../utils/socket.js";
import { connection } from "../connection.js";
import { transactionUpdatedJob } from "../queues.js";

const worker = new Worker<transactionUpdatedJob>(
  "transactionUpdatedQueue",
  async (job) => {
    const { txHash } = job.data;
    console.log("👷 Transaction Update worker processing job", txHash);

    try {
      const cardano = new Cardano();
      await cardano.init();

      // 1) check if transaction has been updated on-chain
      const isUpdated: boolean = await cardano.updatedOnChain(txHash);

      // 2) emit it
      getIO().emit("transactionUpdated", isUpdated);

      return { success: true, isUpdated };
    } catch (error) {
      console.error("❌ Worker error:", error);
      throw error;
    }
  },
  {
    connection,
    concurrency: 1,
  }
);

// Add event listeners for debugging
worker.on("ready", () => {
  console.log("👷 Transaction Update worker is ready");
});

worker.on("error", (err) => {
  console.error("❌ Transaction Update worker error:", err);
});

worker.on("failed", (job, err) => {
  console.error(`❌ Job ${job?.id} failed:`, err);
});

worker.on("completed", (job, result) => {
  console.log(`✅ Job ${job.id} completed:`, result);
});
