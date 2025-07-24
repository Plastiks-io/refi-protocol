import { Validator } from "lucid-cardano";
import dotenv from "dotenv";
dotenv.config();

export const stakeRewardValidator: Validator = {
  type: "PlutusV2",
  script: process.env.STAKE_REWARD_CBOR!,
};

export const refiValidator: Validator = {
  type: "PlutusV2",
  script: process.env.REFI_CBOR!,
};
