import { Job, Worker } from "bullmq";
import { RefiContractJob } from "../queues.js";
import { Cardano } from "../../utils/cardano.js";
import { getIO } from "../../utils/socket.js";
import { connection } from "../connection.js";

const worker = new Worker<RefiContractJob>(
  "updateRefiContractQueue",
  async (job: Job<RefiContractJob>) => {
    const { preId, roadmapId, txHash } = job.data;
    console.log("👷 Refi Contract worker processing job", txHash);
    const cardano = new Cardano();
    await cardano.init();

    // 1) check if transaction has been updated on-chain
    await cardano.updatedOnChain(txHash);

    // 2) fetch the fresh ReFi datum
    const updatedDatum = await cardano.getRoadmapDatum(preId, roadmapId);

    // 3) emit to all clients
    getIO().emit("roadmapUpdated", updatedDatum);

    return { success: true, updatedDatum };
  },
  { connection, concurrency: 1 }
);

// Add event listeners for debugging
worker.on("ready", () => {
  console.log("👷 Refi Contract worker is ready");
});

worker.on("error", (err) => {
  console.error("❌ Refi Contract worker error:", err);
});

worker.on("failed", (job, err) => {
  console.error(`❌ Job ${job?.id} failed:`, err);
});

worker.on("completed", (job, result) => {
  console.log(`✅ Job ${job.id} completed:`, result);
});
