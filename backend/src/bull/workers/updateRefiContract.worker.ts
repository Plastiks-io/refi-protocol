import { Job, Worker } from "bullmq";
import { RefiContractJob } from "../queues.js";
import { Cardano } from "../../utils/cardano.js";
import { getIO } from "../../utils/socket.js";
import { connection } from "../connection.js";

new Worker<RefiContractJob>(
  "updateRefiContractQueue",
  async (job: Job<RefiContractJob>) => {
    const { preId, roadmapId, txHash } = job.data;
    const cardano = new Cardano();
    await cardano.init();

    // 1) check if transaction has been updated on-chain
    await cardano.updatedOnChain(txHash);

    // 2) fetch the fresh ReFi datum
    const updatedDatum = await cardano.getRoadmapDatum(preId, roadmapId);

    // 3) emit to all clients
    getIO().emit("roadmapUpdated", updatedDatum);
  },
  { connection, concurrency: 1 }
);
