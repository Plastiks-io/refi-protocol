import { Validator } from "lucid-cardano";
import config from "../config/environment.js";

export const stakeRewardValidator: Validator = {
  type: "PlutusV2",
  script: config.CONTRACTS.STAKE_REWARD_CBOR!,
};

export const refiValidator: Validator = {
  type: "PlutusV2",
  script: config.CONTRACTS.REFI_CBOR!,
};
