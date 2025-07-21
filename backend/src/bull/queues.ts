// src/bull/queues.ts
import { Queue } from "bullmq";
import { connection } from "./connection.js";

export interface BuyNftJob {
  txHash: string;
  buyerAddress: string;
  preId: string;
  roadmapId: string;
  soldPlasticCredit: number;
}

export interface RefiContractJob {
  preId: string;
  roadmapId: string;
  txHash: string;
}
export interface StakeContractJob {
  txHash: string;
}

export const buyNftQueue = new Queue<BuyNftJob>("buyNftQueue", {
  connection,
});

// New queues
export const updateRefiContractQueue = new Queue<RefiContractJob>(
  "updateRefiContractQueue",
  { connection }
);
export const updateStakeContractQueue = new Queue<StakeContractJob>(
  "updateStakeContractQueue",
  { connection }
);
