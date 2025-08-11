import {
  Lucid,
  Blockfrost,
  Data,
  Constr,
  Assets,
  fromText,
  toText,
  Validator,
  UTxO,
  getAddressDetails,
} from "lucid-cardano";
import { refiValidator, stakeRewardValidator } from "../contract/contracts.js";
import { LenderDatum, RoadmapDatum } from "../types/stake.reward.types.js";
import config from "../config/environment.js";

const initLucid = async () => {
  const lucid = await Lucid.new(
    new Blockfrost(config.BLOCKFROST.URL!, config.BLOCKFROST.PROJECT_ID!),
    "Preprod"
  );
  return lucid;
};

const stakeRewardAddress = await initLucid().then((lucid) =>
  lucid.utils.validatorToAddress(stakeRewardValidator)
);

const refiContractAddress = await initLucid().then((lucid) =>
  lucid.utils.validatorToAddress(refiValidator)
);

// Parses Data → JS RoadmapDatum, expecting 15 fields in the Constr
export function parseRoadmapDatum(data: Data): RoadmapDatum {
  if (data instanceof Constr && data.index === 0) {
    const [
      maybePreId,
      maybeRoadmapId,
      maybeRoadmapName,
      maybeRoadmapDescription,
      maybeProgress,
      maybeAdminsPkh,
      maybePrePkh,
      maybePreSkh,
      maybeTotalPlasticCredits,
      maybeSoldPlasticCredits,
      maybeTotalPlasticTokens,
      maybeSentPlasticTokens,
      maybeTotalPlastic,
      maybeRecoverPlastic,
      maybeCreatedAt,
    ] = data.fields;

    // Validate basic types
    if (
      typeof maybePreId === "string" &&
      typeof maybeRoadmapId === "string" &&
      typeof maybeRoadmapName === "string" &&
      typeof maybeRoadmapDescription === "string" &&
      typeof maybeProgress === "bigint" &&
      Array.isArray(maybeAdminsPkh) &&
      typeof maybePrePkh === "string" &&
      typeof maybePreSkh === "string" &&
      typeof maybeTotalPlasticCredits === "bigint" &&
      typeof maybeSoldPlasticCredits === "bigint" &&
      typeof maybeTotalPlasticTokens === "bigint" &&
      typeof maybeSentPlasticTokens === "bigint" &&
      typeof maybeTotalPlastic === "bigint" &&
      typeof maybeRecoverPlastic === "bigint" &&
      typeof maybeCreatedAt === "string"
    ) {
      // Parse admin PKHs array
      const adminsPkh: string[] = [];
      for (const adminPkh of maybeAdminsPkh) {
        if (typeof adminPkh === "string") {
          adminsPkh.push(adminPkh);
        } else {
          throw new Error("Invalid admin PKH format in array");
        }
      }

      return {
        preId: toText(maybePreId),
        roadmapId: toText(maybeRoadmapId),
        roadmapName: toText(maybeRoadmapName),
        roadmapDescription: toText(maybeRoadmapDescription),
        progress: maybeProgress,
        adminsPkh,
        prePkh: maybePrePkh,
        preSkh: maybePreSkh,
        totalPlasticCredits: maybeTotalPlasticCredits,
        soldPlasticCredits: maybeSoldPlasticCredits,
        totalPlasticTokens: maybeTotalPlasticTokens,
        sentPlasticTokens: maybeSentPlasticTokens,
        totalPlastic: maybeTotalPlastic,
        recoverPlastic: maybeRecoverPlastic,
        createdAt: toText(maybeCreatedAt),
      };
    }
  }
  throw new Error("Invalid roadmap datum format");
}

const ptPolicyId = "d4fece6b39f7cd78a3f036b2ae6508c13524b863922da80f68dd9ab7";
const ptTokenName = fromText("PLASTIK");
const ptAssetUnit = ptPolicyId + ptTokenName;

const usdmPolicyId = "d4fece6b39f7cd78a3f036b2ae6508c13524b863922da80f68dd9ab7";
const usdmTokenName = fromText("USDM");
const usdmAssetUnit = usdmPolicyId + usdmTokenName;

const precisionFactor = 1_000_000n;

//
// 3) Helper: get the current wallet’s PubKeyHash
//

export const getPubKeyHash = async (lucid: Lucid): Promise<string> => {
  const address = await lucid.wallet.address();
  const { paymentCredential } = getAddressDetails(address);
  return paymentCredential?.hash || "";
};

//
// 4) Updated “LenderAction” builder: now includes AdminWithdraw and AdminReturn
//

export const buildLenderAction = (action: {
  type: "Deposit" | "Withdraw" | "Redeem" | "FundPlastikToEscrow" | "FundUSDM";
  amount?: bigint;
}): Constr<Data> => {
  switch (action.type) {
    case "Deposit":
      // Haskell: “Deposit” is the first constructor → index 0, no fields
      return new Constr(0, []);
    case "Withdraw":
      if (action.amount === undefined) {
        throw new Error("Withdraw requires amount");
      }
      // Haskell: 2nd constructor → index 3, one Integer field
      return new Constr(1, [action.amount]);
    case "Redeem":
      // Haskell: 3rd constructor → index 4, no fields
      return new Constr(2, []);
    case "FundPlastikToEscrow":
      if (action.amount === undefined) {
        throw new Error("FundPlastikToEscrow requires amount");
      }
      // Haskell: 4th constructor → index 5, one Integer field
      return new Constr(3, [action.amount]);
    case "FundUSDM":
      if (action.amount === undefined) {
        throw new Error("FundUSDM requires amount");
      }
      // Haskell: 5th constructor → index 6, one Integer field
      return new Constr(4, [action.amount]);
    default:
      throw new Error("Unknown action type");
  }
};

// Builds a Constr<Data> matching: LenderDatum adminPkh totalPT totalReward lenders
export function buildLenderDatum(datum: LenderDatum): Constr<Data> {
  // Convert each lender entry ([pkh, [balance, rewardDebt]]) → Constr(0, [pkh, Constr(0, [balance, rewardDebt])])
  const lendersData = datum.lenders.map(
    ([pkh, [balance, rewardDebt]]) =>
      new Constr(0, [pkh, new Constr(0, [balance, rewardDebt])])
  );
  // Now emit a Constr with index 0 (since Haskell’s single data type LenderDatum gets 0 index)
  // and fields: [adminPkh, totalPT, totalReward, lendersData].
  return new Constr(0, [
    datum.adminsPkh,
    datum.totalPT,
    datum.totalReward,
    lendersData,
  ]);
}

// Parses Data → JS LenderDatum, expecting 4 fields in the Constr
export function parseLenderDatum(data: Data): LenderDatum {
  if (data instanceof Constr && data.index === 0) {
    const [maybeAdminsPkh, maybeTotalPT, maybeTotalReward, maybeLendersData] =
      data.fields;

    if (
      Array.isArray(maybeAdminsPkh) &&
      typeof maybeTotalPT === "bigint" &&
      typeof maybeTotalReward === "bigint" &&
      Array.isArray(maybeLendersData)
    ) {
      const adminsPkh: string[] = [];
      for (const pkh of maybeAdminsPkh) {
        if (typeof pkh === "string") {
          adminsPkh.push(pkh);
        } else {
          throw new Error("Invalid admin PKH format in array");
        }
      }
      const lenders: [string, [bigint, bigint]][] = [];

      for (const lenderData of maybeLendersData) {
        if (lenderData instanceof Constr && lenderData.index === 0) {
          const [pkh, tupleData] = lenderData.fields;

          if (
            typeof pkh === "string" &&
            tupleData instanceof Constr &&
            tupleData.index === 0
          ) {
            const [balance, rewardDebt] = tupleData.fields;
            if (typeof balance === "bigint" && typeof rewardDebt === "bigint") {
              lenders.push([pkh, [balance, rewardDebt]]);
            }
          }
        }
      }

      return {
        adminsPkh: adminsPkh,
        totalPT: maybeTotalPT,
        totalReward: maybeTotalReward,
        lenders,
      };
    }
  }
  throw new Error("Invalid datum format");
}
