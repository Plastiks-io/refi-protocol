export type ProjectDatum = {
  preId: string; // decode string
  roadmapId: string; // decode string
  roadmapName: string; // decode string
  roadmapDescription: string; // decode string
  progress: number;
  prePkh: string;
  adminPkh: string;
  totalPlasticCredits: number;
  soldPlasticCredits: number;
  totalPlasticTokens: number;
  sentPlasticTokens: number;
  totalPlastic: number;
  recoveredPlastic: number;
  txHash?: string;
};

export type InitializeRoadmapRequest = {
  preId: string;
  roadmapId: string;
  roadmapName: string;
  roadmapDescription: string;
  prePkh: string;
  preSkh: string;
  totalPlasticCredits: number;
  totalPlasticTokens: number;
  totalPlastic: number;
};

export type UpdateRoadmapRequest = {
  preId: string;
  roadmapId: string;
  soldPlasticCredit: number;
};

export type ReleaseFundsRequest = {
  preId: string;
  roadmapId: string;
};

export type QueryTransaction = {
  txHash: string;
};
