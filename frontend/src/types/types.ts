import { BrowserWallet } from "@meshsdk/core";
import { Network } from "lucid-cardano";

export type Wallet = {
  id: string;
  name: string;
  icon: string;
  version: string;
};

// 1. MeshBrowserWallet is what enable() returns
export type MeshBrowserWallet = Awaited<
  ReturnType<typeof BrowserWallet.enable>
>;
// 2. The CIP‑30 API you actually call against
export type MeshWalletApi = MeshBrowserWallet["walletInstance"];

export interface NetworkConfig {
  projectId: string;
  baseUrl: string;
  lucidNetwork: Network;
  cardanoScanUrl: string;
}

// Add this interface at the top of your file or in a separate types file
export interface GovernanceStats {
  currentRetirementRate: string;
  roundDuration: string;
  totalVotes: string;
  totalPlastikVoted: string;
  hasActiveProposal: boolean;
  activeProposal?: {
    proposalId: bigint;
    retirementPercentage: bigint;
    proposerPkh: string;
    createdAt: bigint;
    expiresAt: bigint;
    votesFor: bigint;
    votesAgainst: bigint;
    executed: boolean;
  };
}
