export type ProjectDatum = {
  preId: string;
  roadmapId: string;
  roadmapName: string;
  roadmapDescription: string;
  progress: number;
  preAddress: string;
  totalPlasticCredits: number;
  soldPlasticCredits: number;
  totalPlasticTokens: number;
  sentPlasticTokens: number;
  totalPlastic: number;
  recoveredPlastic: number;
  createdAt: string;
  status: string;
};

export type InitializeRoadmapRequest = {
  preId: string;
  roadmapId: string;
  roadmapName: string;
  roadmapDescription: string;
  prePkh: string;
  preSkh: string;
  totalPlasticCredits: number;
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
  outputIndex: number;
};

export type ArchivedRoadmapRequest = {
  preId: string;
  roadmapId: string;
};
