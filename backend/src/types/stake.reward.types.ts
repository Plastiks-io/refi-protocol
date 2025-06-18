export type LenderDatum = {
  adminsPkh: string[]; // first field
  totalPT: bigint; // second field
  totalReward: bigint; // third field
  lenders: [string, [bigint, bigint]][]; // fourth field
};

// Roadmap Datum Interface
export interface RoadmapDatum {
  preId: string;
  roadmapId: string;
  roadmapName: string;
  roadmapDescription: string;
  progress: bigint;
  adminsPkh: string[]; // Array of admin PKHs
  prePkh: string;
  preSkh: string;
  totalPlasticCredits: bigint;
  soldPlasticCredits: bigint;
  totalPlasticTokens: bigint;
  sentPlasticTokens: bigint;
  totalPlastic: bigint;
  recoverPlastic: bigint;
  createdAt: string;
}
