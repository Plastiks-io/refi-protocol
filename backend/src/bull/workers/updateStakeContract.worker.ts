// updateStakeContract.worker.ts
import { Worker } from "bullmq";
import { Cardano } from "../../utils/cardano.js";
import { getIO } from "../../utils/socket.js";
import { connection } from "../connection.js";
import { StakeContractJob } from "../queues.js";

const worker = new Worker<StakeContractJob>(
  "updateStakeContractQueue",
  async (job) => {
    const { txHash } = job.data;
    console.log("👷 updateStakeContractQueue worker processing job", txHash);

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
  },
  {
    connection,
    concurrency: 1,
  }
);

// Add event listeners for debugging
worker.on("ready", () => {
  console.log("👷 Stake Contract worker is ready");
});

worker.on("error", (err) => {
  console.error("❌ updateStakeContractQueue worker error:", err);
});

worker.on("failed", (job, err) => {
  console.error(`❌ Job ${job?.id} failed:`, err);
});

worker.on("completed", (job, result) => {
  console.log(`✅ Job ${job.id} completed:`, result);
});
