// src/bull/queues.ts
import { Queue } from "bullmq";
import { connection } from "./connection.js";

export interface BuyNftJob {
  txHash: string;
  buyerAddress: string;
  preId: string;
  roadmapId: string;
  soldPlasticCredit: number;
  currentProgress?: number;
}

export interface UpdateFundsJob {
  preId: string;
  roadmapId: string;
  currentUSDM: number;
  txHash: string;
}
export interface StakeContractJob {
  txHash: string;
  stakedNumber: number;
  rewardNumber: number;
}

export interface transactionUpdatedJob {
  txHash: string;
}

export const buyNftQueue = new Queue<BuyNftJob>("buyNftQueue", {
  connection,
});

// New queues
export const updateFundsQueue = new Queue<UpdateFundsJob>("updateFundsQueue", {
  connection,
});

export const updateStakeContractQueue = new Queue<StakeContractJob>(
  "updateStakeContractQueue",
  { connection }
);

export const transactionUpdatedQueue = new Queue<transactionUpdatedJob>(
  "transactionUpdatedQueue",
  { connection }
);
