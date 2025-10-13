import { Blockfrost, Constr, Data, Lucid, Validator } from "lucid-cardano";
import config from "../config/environment.js";
import { Request, Response } from "express";

const initializeLucid = async () => {
  const lucid = await Lucid.new(
    new Blockfrost(config.BLOCKFROST.URL!, config.BLOCKFROST.PROJECT_ID!),
    "Preprod"
  );
  return lucid;
};

const governanceValidator: Validator = {
  type: "PlutusV2",
  script: config.CONTRACTS.GOVERNANCE_CBOR!,
};

function governanceAddress(lucid: Lucid): string {
  if (!lucid) throw new Error("Lucid not initialized");
  return lucid.utils.validatorToAddress(governanceValidator);
}

/**
 * Get the current governance state from the blockchain
 */
async function getGovernanceState(): Promise<Constr<Data>> {
  const lucid = await initializeLucid();
  const utxos = await lucid.utxosAt(governanceAddress(lucid));
  const govUtxo = utxos.find(
    (u) => u.datum && Data.from(u.datum) instanceof Constr
  );
  if (!govUtxo) throw new Error("No governance UTxO at script");

  return Data.from(govUtxo.datum!) as Constr<Data>;
}

/**
 * Recursively convert BigInt values to strings so JSON.stringify won't throw.
 * Leaves other primitives unchanged.
 */
function serializeBigInts(obj: any): any {
  if (typeof obj === "bigint") return obj.toString();
  if (Array.isArray(obj)) return obj.map(serializeBigInts);
  if (obj && typeof obj === "object") {
    // Keep the prototype-less plain object output (avoid copying circular refs)
    const out: any = {};
    for (const [k, v] of Object.entries(obj)) {
      out[k] = serializeBigInts(v);
    }
    return out;
  }
  return obj;
}

/**
 * Get governance statistics from the smart contract datum
 */
async function getGovernanceStats(req: Request, res: Response): Promise<void> {
  try {
    const governanceState = await getGovernanceState();

    // The top-level fields in your Constr
    const [currRate, maybeProp, counter, minPeriod, quorum] =
      governanceState.fields as [bigint, Constr<Data>, bigint, bigint, bigint];

    const currentRetirementRate = `${currRate}%`;

    // defaults
    let roundDuration = "No active voting round";
    let totalVotes = "0";
    let totalPlastikVoted = "0"; // Placeholder - depends on contract data shape
    let hasActiveProposal = false;
    let activeProposal: any = undefined;

    // If there's an active proposal (Maybe is Constr with index 1)
    if (maybeProp && maybeProp.index === 1) {
      hasActiveProposal = true;
      const propConstr = maybeProp.fields[0] as Constr<Data>;

      const [
        propIdData,
        retirementPctData,
        proposerData,
        createdAtData,
        expiresAtData,
        votesForData,
        votesAgainstData,
        allVotesData,
        executedData,
      ] = propConstr.fields;

      // keep original bigints for calculations, then we'll serialize at the end
      const proposalId = propIdData as bigint;
      const retirementPercentage = retirementPctData as bigint;
      const proposerPkh = proposerData as string;
      const createdAt = createdAtData as bigint;
      const expiresAt = expiresAtData as bigint;
      const votesFor = votesForData as bigint;
      const votesAgainst = votesAgainstData as bigint;
      const executed = (executedData as Constr<Data>).index === 1;

      // Calculate human readable round duration (formatted)
      // createdAt / expiresAt are bigints representing epoch ms (based on your sample)
      const startDate = new Date(Number(createdAt));
      const endDate = new Date(Number(expiresAt));
      const formattedStartDate = startDate.toLocaleDateString("en-US", {
        day: "numeric",
        month: "short",
      });
      const formattedEndDate = endDate.toLocaleDateString("en-US", {
        day: "numeric",
        month: "short",
      });
      roundDuration = `${formattedStartDate} - ${formattedEndDate}`;

      // total votes as a string with locale separators
      const totalVoteCount = votesFor + votesAgainst; // bigint
      // BigInt.prototype.toLocaleString exists and returns string
      totalVotes = totalVoteCount.toLocaleString();

      // Placeholder calculation for totalPlastikVoted — keep as string
      totalPlastikVoted = `${(Number(totalVoteCount) * 100).toLocaleString()}`;

      activeProposal = {
        proposalId,
        retirementPercentage,
        proposerPkh,
        createdAt, // bigint
        expiresAt, // bigint
        votesFor,
        votesAgainst,
        executed,
      };
    }

    // Build payload and serialize bigints safely
    const payload = {
      currentRetirementRate,
      roundDuration,
      totalVotes,
      totalPlastikVoted,
      hasActiveProposal,
      activeProposal,
    };

    const safePayload = serializeBigInts(payload);

    res.status(200).json(safePayload);
    return;
  } catch (error) {
    console.error("Error getting governance stats:", error);
    // Return default values if contract is not initialized or accessible
    res.status(404).json({
      success: false,
      message: "Contract not initialized or accessible",
    });
    return;
  }
}

export { getGovernanceStats, getGovernanceState };
