import { Job, Worker } from "bullmq";
import { UpdateFundsJob } from "../queues.js";
import { Cardano } from "../../utils/cardano.js";
import { getIO } from "../../utils/socket.js";
import { connection } from "../connection.js";

const worker = new Worker<UpdateFundsJob>(
  "updateFundsQueue",
  async (job: Job<UpdateFundsJob>) => {
    const { preId, roadmapId, currentUSDM, txHash } = job.data;
    console.log("👷 Update Funds worker processing job", txHash);
    const cardano = new Cardano();
    await cardano.init();

    // 1) fetch the fresh datum

    const updatedDatum = await cardano.fundsUpdated(
      preId,
      roadmapId,
      txHash,
      currentUSDM
    );

    // 2) emit to all clients
    getIO().emit("fundsUpdated", updatedDatum);

    return { success: true, updatedDatum };
  },
  { connection, concurrency: 1 }
);

// Add event listeners for debugging
worker.on("ready", () => {
  console.log("👷 Update Funds worker is ready");
});

worker.on("error", (err) => {
  console.error("❌ Update Funds worker error:", err);
});

worker.on("failed", (job, err) => {
  console.error(`❌ Job ${job?.id} failed:`, err);
});

worker.on("completed", (job, result) => {
  console.log(`✅ Job ${job.id} completed:`, result);
});
