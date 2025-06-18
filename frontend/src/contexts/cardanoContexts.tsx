// src/contexts/CardanoContext.tsx
import React, { createContext, useContext, useState, useEffect } from "react";
import { cardanoClient } from "@/services/cardano";
import type { BrowserWallet } from "@meshsdk/core";

type State = {
  data: { staked: bigint; rewardDebt: bigint } | null;
  refresh: () => Promise<void>;
};

const CardanoCtx = createContext<State | null>(null);

export const CardanoProvider: React.FC<{
  wallet: BrowserWallet | null;
  children: React.ReactNode;
}> = ({ wallet, children }) => {
  const [data, setData] = useState<State["data"]>(null);

  const refresh = async () => {
    if (!wallet) return;
    const result = await cardanoClient.getStakeAndReward(wallet);
    setData(result);
  };

  // fetch once on mount (and whenever wallet changes)
  useEffect(() => {
    refresh().catch(console.error);
  }, [wallet]);

  return (
    <CardanoCtx.Provider value={{ data, refresh }}>
      {children}
    </CardanoCtx.Provider>
  );
};

export const useCardanoData = () => {
  const ctx = useContext(CardanoCtx);
  if (!ctx) throw new Error("CardanoProvider missing");
  return ctx;
};
