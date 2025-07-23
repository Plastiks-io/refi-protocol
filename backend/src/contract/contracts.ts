// src/contract/contracts.ts

function getRequiredEnvVar(name: string): string {
  const value = process.env[name];
  if (!value) {
    throw new Error(`Missing environment variable: ${name}`);
  }
  return value;
}

export const stakeRewardValidator = {
  type: "PlutusV2" as const,
  script: getRequiredEnvVar("STAKE_REWARD_CBOR"),
};

export const refiValidator = {
  type: "PlutusV2" as const,
  script: getRequiredEnvVar("REFI_CBOR"),
};
