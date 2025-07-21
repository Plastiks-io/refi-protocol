import { Job, Worker } from "bullmq";
import { Cardano } from "../../utils/cardano.js";
import { getIO } from "../../utils/socket.js";
import { connection } from "../connection.js";
import { StakeContractJob } from "../queues.js";

new Worker<StakeContractJob>(
  "updateStakeContractQueue",
  async (job: Job<StakeContractJob>) => {
    const { txHash } = job.data;
    const cardano = new Cardano();
    await cardano.init();

    // 2) check if transaction has been updated on-chain
    const isUpdated = await cardano.updatedOnChain(txHash);

    // 3) emit it
    getIO().emit("stakeContractUpdated", isUpdated);
  },
  { connection, concurrency: 1 }
);
