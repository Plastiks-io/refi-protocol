import {
  Validator,
  Lucid,
  Blockfrost,
  Data,
  toText,
  fromText,
  Constr,
  Network,
  Assets,
  UTxO,
} from "lucid-cardano";
import { Transaction, TransactionType } from "../models/transaction.model.js";
import {
  buildLenderAction,
  buildLenderDatum,
  getPubKeyHash,
  parseLenderDatum,
  parseRoadmapDatum,
} from "../controllers/stakeReward.controller.js";
import { LenderDatum } from "../types/stake.reward.types.js";
import { ProjectDatum } from "../types/roadmap.types.js";
import config from "../config/environment.js";
import { Decimal } from "decimal.js";

export interface NetworkConfig {
  projectId: string;
  baseUrl: string;
  lucidNetwork: Network;
}

export class Cardano {
  public lucidInstance: Lucid | null = null;
  public policyId: string;
  public ptAssetName: string;
  public usdmAssetName: string;
  public stakeRewardValidator: Validator;
  public refiValidator: Validator;

  // Define supported network configs
  private static NETWORKS: NetworkConfig = {
    baseUrl: config.BLOCKFROST.URL!,
    projectId: config.BLOCKFROST.PROJECT_ID!,
    lucidNetwork: "Preprod",
  };

  constructor() {
    // Initialize asset units from environment variables
    this.policyId = config.ASSETS.POLICY_ID!;
    this.ptAssetName = config.ASSETS.PLASTIC_TOKEN_NAME!;
    this.usdmAssetName = config.ASSETS.USDM_TOKEN_NAME!;
    // Validate required environment variables
    if (!this.policyId || !this.ptAssetName || !this.usdmAssetName) {
      throw new Error(
        "Missing required environment variables for Cardano asset configuration"
      );
    }

    // Initialize validators
    const stakeRewardCbor = config.CONTRACTS.STAKE_REWARD_CBOR;
    const refiCbor = config.CONTRACTS.REFI_CBOR;

    if (!stakeRewardCbor) {
      throw new Error("STAKE_REWARD_CBOR environment variable is required");
    }
    if (!refiCbor) {
      throw new Error("VITE_REFI_CBOR environment variable is required");
    }

    this.stakeRewardValidator = {
      type: "PlutusV2",
      script: stakeRewardCbor,
    };

    this.refiValidator = {
      type: "PlutusV2",
      script: refiCbor,
    };

    // Validate network configuration
    const cfg = Cardano.NETWORKS;
    if (!cfg.projectId || !cfg.baseUrl) {
      throw new Error(
        "Missing Blockfrost configuration in environment variables"
      );
    }
  }

  public async init(): Promise<void> {
    const cfg = Cardano.NETWORKS;
    if (!cfg) {
      throw new Error(`Unsupported network.`);
    }

    if (!this.lucidInstance) {
      const { projectId, baseUrl, lucidNetwork } = cfg;
      if (!projectId || !baseUrl) {
        throw new Error(
          "Missing Blockfrost configuration for selected network."
        );
      }

      this.lucidInstance = await Lucid.new(
        new Blockfrost(baseUrl, projectId),
        lucidNetwork
      );
    }
  }

  /** Ensures Lucid is initialized */
  public getLucid(): Lucid {
    if (!this.lucidInstance) {
      throw new Error("Lucid not initialized: call init(networkId) first.");
    }
    return this.lucidInstance;
  }

  public get stakeRewardAddress(): string {
    if (!this.lucidInstance) throw new Error("Lucid not initialized");
    return this.lucidInstance.utils.validatorToAddress(
      this.stakeRewardValidator
    );
  }

  public get refiContractAddress(): string {
    if (!this.lucidInstance) throw new Error("Lucid not initialized");
    return this.lucidInstance.utils.validatorToAddress(this.refiValidator);
  }

  /**
   * Check if a transaction is confirmed on-chain.
   */
  public async checkTxConfirmed(txHash: string): Promise<boolean> {
    const lucid = this.getLucid();
    try {
      const txResult = await lucid.awaitTx(txHash);
      return txResult;
    } catch (err) {
      console.error("Error fetching transaction:", err);
      return false;
    }
  }

  public async sendPcToken(
    address: string,
    amount: number,
    roadmapId: string
  ): Promise<string> {
    if (!address || !amount) {
      throw new Error("Address and amount are required");
    }

    try {
      const lucid = this.getLucid();
      const seed = config.WALLETS.PC_WALLET!;
      lucid.selectWalletFromSeed(seed);
      const { paymentCredential } = lucid.utils.getAddressDetails(
        await lucid.wallet.address()
      );

      // Parse the minting policy from env and convert to Script format
      const mintingPolicy = lucid.utils.nativeScriptFromJson({
        type: "all",
        scripts: [
          {
            type: "sig",
            keyHash: paymentCredential?.hash ?? "",
          },
        ],
      });
      const policyId = lucid.utils.mintingPolicyToId(mintingPolicy);

      const timestamp = Date.now();
      const tokenName = fromText(`PLASTIC_CREDIT_${timestamp}`);
      const metadata = {
        [policyId]: {
          [tokenName]: {
            name: "PLASTIC CREDIT",
            image: "ipfs://QmP6mKWsUExK1emB7K9bxSRjdRLaqoysdutTsfYg6PDGUy",
            description: "This Token was minted for Plastik",
            mediaType: "image/png",
          },
        },
      };

      // Custom metadata for buy amount (using label 674 for custom data)
      const buyAmountMetadata = {
        674: {
          msg: [
            `Plastic Credits Purchased: ${amount}`,
            `Purchase Date: ${new Date().toISOString()}`,
            `Roadmap ID: ${roadmapId}`,
          ],
        },
      };

      const unit = `${policyId}${tokenName}`;

      const tx = await lucid
        .newTx()
        .mintAssets({ [unit]: 1n })
        .attachMintingPolicy(mintingPolicy)
        .attachMetadata(721, metadata) // Standard NFT metadata
        .attachMetadata(674, buyAmountMetadata) // Custom purchase metadata
        .payToAddress(address, { [unit]: 1n })
        .complete();

      const signedTx = await tx.sign().complete();
      const txHash = await signedTx.submit();

      // Create a new transaction in the database
      await Transaction.create({
        txDate: new Date(),
        txFee: 2,
        amount,
        roadmapId,
        assetId: `${policyId}.${tokenName}`,
        hash: txHash,
        type: TransactionType.Sold,
      });

      return txHash;
    } catch (err) {
      if (err instanceof Error) {
        console.log("Something went wrong during sent P.C", err.message);
      }
      throw err;
    }
  }
  public async updateRoadmap(
    preId: string,
    roadmapId: string,
    soldPlasticCredit: number
  ): Promise<string> {
    try {
      const lucid = this.getLucid();
      const adminSeed = config.WALLETS.ADMIN_SEED!;
      lucid.selectWalletFromSeed(adminSeed);
      const adminAddress = await lucid.wallet.address();

      // Configure Decimal for high precision
      Decimal.set({ precision: 50, rounding: 4 }); // 50 decimal places, round half up

      // Find matching UTXO at refi contract
      const utxos = await lucid.utxosAt(this.refiContractAddress);
      const matchedUtxo = utxos.find((u) => {
        if (!u.datum) return false;
        const d = Data.from(u.datum) as Constr<Data>;
        return (
          toText(d.fields[0] as string) === preId &&
          toText(d.fields[1] as string) === roadmapId
        );
      });
      if (!matchedUtxo?.datum) throw new Error("No matching roadmap UTxO");

      const old = parseRoadmapDatum(Data.from(matchedUtxo.datum));
      const newSold = old.soldPlasticCredits + BigInt(soldPlasticCredit);

      // Use Decimal for ALL precise calculations
      const newSoldDecimal = new Decimal(newSold.toString());
      const totalCreditsDecimal = new Decimal(
        old.totalPlasticCredits.toString()
      );
      const totalTokensDecimal = new Decimal(old.totalPlasticTokens.toString());
      const totalPlasticDecimal = new Decimal(old.totalPlastic.toString());

      // Calculate the actual progress percentage (keep as decimal for precision)
      const progressPercentageDecimal = newSoldDecimal.div(totalCreditsDecimal);

      // Calculate progress in basis points (for storage)
      const progressBasisPoints = progressPercentageDecimal.mul(10000);
      const progress = BigInt(progressBasisPoints.toFixed(0));

      // Calculate newSent and recovered using the precise percentage (NOT basis points)
      const newSentDecimal = progressPercentageDecimal.mul(totalTokensDecimal);
      const recoveredDecimal =
        progressPercentageDecimal.mul(totalPlasticDecimal);

      // Round to nearest whole number for final values
      const newSent = BigInt(newSentDecimal.toFixed(0));
      const recovered = BigInt(recoveredDecimal.toFixed(0));

      console.log("Debug calculations:");
      console.log("newSold:", newSold.toString());
      console.log("totalCredits:", old.totalPlasticCredits.toString());
      console.log("progressPercentage:", progressPercentageDecimal.toString());
      console.log("progressBasisPoints:", progressBasisPoints.toString());
      console.log("totalTokens:", old.totalPlasticTokens.toString());
      console.log("newSentDecimal:", newSentDecimal.toString());
      console.log("newSent:", newSent.toString());
      console.log("recoveredDecimal:", recoveredDecimal.toString());
      console.log("recovered:", recovered.toString());

      const updatedDatum = new Constr(0, [
        fromText(old.preId),
        fromText(old.roadmapId),
        fromText(old.roadmapName),
        fromText(old.roadmapDescription),
        BigInt(progress),
        old.adminsPkh,
        old.prePkh,
        old.preSkh,
        BigInt(old.totalPlasticCredits),
        BigInt(newSold),
        BigInt(old.totalPlasticTokens),
        BigInt(newSent),
        BigInt(old.totalPlastic),
        BigInt(recovered),
        fromText(old.createdAt),
      ]);

      // Prepare reward contract UTxO
      const stakeUtxos = await lucid.utxosAt(this.stakeRewardAddress);
      if (stakeUtxos.length === 0) throw new Error("No stake UTxOs found");
      const stakeUtxo = stakeUtxos[0];
      if (!stakeUtxo.datum) throw new Error("Missing stake datum");
      const lender = parseLenderDatum(Data.from(stakeUtxo.datum));

      const adminPkh = await getPubKeyHash(lucid);
      if (!lender.adminsPkh.includes(adminPkh)) throw new Error("Unauthorized");

      const ptUnit = `${this.policyId}${this.ptAssetName}`;
      const needPT = BigInt(soldPlasticCredit) * 80n;
      // sum PT in this single UTxO
      const contractPT = stakeUtxo.assets[ptUnit] || 0n;

      // Collect any extra from admin if contractPT insufficient
      let extraAdminUtxo;
      if (contractPT < needPT) {
        const adminUtxos = await lucid.utxosAt(adminAddress);
        extraAdminUtxo = adminUtxos.find(
          (u) => (u.assets[ptUnit] || 0n) >= needPT - contractPT
        );
        if (!extraAdminUtxo) throw new Error("Admin has insufficient PT");
      }

      // Build redeemers and new lender datum
      const refiRedeemer = Data.to(new Constr(0, [progress]));
      const lenderRedeemer = Data.to(
        buildLenderAction({ type: "FundPlastikToEscrow", amount: needPT })
      );
      // 1. Compute the *new* reward from this sale only:
      const precision = 1_000_000n;
      const rewardMicro = (BigInt(soldPlasticCredit) * precision * 2n) / 100n;
      // ▸ ΔR in “micro‑USDM” units

      // 2. Distribute ΔR proportionally to existing stakes:
      const updatedLenders = lender.lenders.map(
        ([pk, [bal, oldDebt]]): [string, [bigint, bigint]] => {
          const share =
            lender.totalPT > 0n
              ? (bal * rewardMicro) / lender.totalPT // bal/T_old * ΔR
              : 0n;
          return [pk, [bal, oldDebt + share]];
        }
      );

      // 3. Update your LenderDatum
      const newLenderDatum: LenderDatum = {
        adminsPkh: lender.adminsPkh,
        totalPT: lender.totalPT, // stakes haven’t changed
        totalReward: lender.totalReward + rewardMicro,
        lenders: updatedLenders,
      };

      const usdmAssetUnit: string = `${this.policyId}${this.usdmAssetName}`;
      // Build assets for refi and stake outputs
      const refiAssets: Assets = {
        lovelace: matchedUtxo.assets.lovelace,
        [ptUnit]: (matchedUtxo.assets[ptUnit] || 0n) + needPT,
      };
      if (matchedUtxo.assets[usdmAssetUnit]) {
        refiAssets[usdmAssetUnit] = matchedUtxo.assets[usdmAssetUnit];
      }

      const stakeAssets: Assets = {
        [usdmAssetUnit]: (stakeUtxo.assets[usdmAssetUnit] || 0n) + rewardMicro,
      };
      if (stakeUtxo.assets[ptUnit] - needPT >= 0n) {
        stakeAssets[ptUnit] = (stakeUtxo.assets[ptUnit] || 0n) - needPT;
      }
      // console.log(stakeAssets);

      // Build Tx
      let txBuilder = lucid
        .newTx()
        // refi update
        .collectFrom([matchedUtxo], refiRedeemer)
        .attachSpendingValidator(this.refiValidator)
        .payToContract(
          lucid.utils.validatorToAddress(this.refiValidator),
          { inline: Data.to(updatedDatum) },
          refiAssets
        )
        // stake payout
        .collectFrom([stakeUtxo], lenderRedeemer);

      // if extra admin funds needed, collect from admin UTxO
      if (extraAdminUtxo) {
        txBuilder = txBuilder.collectFrom([extraAdminUtxo]);
      }

      txBuilder = txBuilder
        .attachSpendingValidator(this.stakeRewardValidator)
        .payToContract(
          this.stakeRewardAddress,
          { inline: Data.to(buildLenderDatum(newLenderDatum)) },
          stakeAssets
        )
        .addSigner(adminAddress);

      const tx = await txBuilder.complete();
      const signed = await tx.sign().complete();
      const hash = await signed.submit();

      // after successfully updating roadmap check if roadmap is complete then save it into DB with name of completed Roadmap
      const ptAssetUnit = `${this.policyId}${this.ptAssetName}`;
      if (progress === 10000n) {
        await Transaction.create({
          txDate: new Date(),
          txFee: 2,
          amount: Number(needPT),
          roadmapId,
          assetId: ptAssetUnit,
          hash: hash,
          type: TransactionType.Roadmap,
        });
      }
      return hash;
    } catch (err) {
      if (err instanceof Error) {
        throw new Error(err.message);
      }
      throw new Error("Unknown error");
    }
  }

  public async refundAda(address: string, amount: number): Promise<string> {
    const lucid = this.getLucid();
    // Select your centralized admin wallet (from seed)
    lucid.selectWalletFromSeed(config.WALLETS.ADMIN_SEED!);

    // Calculate exact ADA amount to refund:
    // You can store it in job.data or re‑derive from the txHash UTxO.
    const refundAmount = BigInt(amount) * 1_000_000n;

    const tx = await lucid
      .newTx()
      .payToAddress(address, { lovelace: refundAmount })
      .complete();

    const signed = await tx.sign().complete();
    const hash = await signed.submit();
    return hash;
  }

  public async getRoadmapDatum(
    preId: string,
    roadmapId: string
  ): Promise<ProjectDatum> {
    const lucid = this.getLucid();
    const utxos: UTxO[] = await lucid.utxosAt(this.refiContractAddress);

    // 1. Find the UTxO whose datum’s roadmapId field matches
    for (const utxo of utxos) {
      if (!utxo.datum) continue;

      const decoded = Data.from(utxo.datum) as Constr<Data>;
      const fields = decoded.fields;

      // Field layout (0: preId, 1: roadmapId, 2: name, 3: desc, 4: progress, …)
      const thisRoadmapId = toText(fields[1] as string);
      if (thisRoadmapId !== roadmapId) continue;

      // 2. Extract and convert each field
      const rawProgress = Number(fields[4] as bigint);
      const progress = rawProgress > 0 ? rawProgress / 100 : 0;

      const paymentCred = lucid.utils.keyHashToCredential(fields[6] as string);
      const stakeCred = lucid.utils.keyHashToCredential(fields[7] as string);
      const preAddress = lucid.utils.credentialToAddress(
        paymentCred,
        stakeCred
      );

      const precisionFactor = 1_000_000n; // 1 PC = 1,000,000 micro PC
      const ptAssetUnit = `${this.policyId}${this.ptAssetName}`;
      const fundsMissing =
        ((utxo.assets[ptAssetUnit] ?? 0n) * precisionFactor) / 100n;

      const usdmAssetUnit: string = `${this.policyId}${this.usdmAssetName}`;
      const fundsDistributed =
        utxo.assets[usdmAssetUnit] ?? 0n / precisionFactor;

      // 3. Build and return the ProjectDatum
      return {
        preId: toText(fields[0] as string),
        roadmapId: thisRoadmapId,
        roadmapName: toText(fields[2] as string),
        roadmapDescription: toText(fields[3] as string),
        progress,
        preAddress,
        totalPlasticCredits: Number(fields[8] as bigint),
        soldPlasticCredits: Number(fields[9] as bigint),
        totalPlasticTokens: Number(fields[10] as bigint),
        sentPlasticTokens: Number(fields[11] as bigint),
        totalPlastic: Number(fields[12] as bigint),
        recoveredPlastic: Number(fields[13] as bigint),
        createdAt: toText(fields[14] as string),
        status: "active",
        fundsMissing: fundsMissing.toString(),
        fundsDistributed: fundsDistributed.toString(),
      };
    }

    throw new Error(
      `Roadmap ${roadmapId} not found at ${this.refiContractAddress}`
    );
  }

  /**
   * Poll Cardano transaction via Blockfrost until confirmed, timeout after 5 minutes.
   */
  public async updatedOnChain(txHash: string): Promise<boolean> {
    const { baseUrl, projectId } = Cardano.NETWORKS;
    const timeoutMs = 300_000; // 5 minutes
    const pollIntervalMs = 10_000; // 10 seconds
    const start = Date.now();

    while (Date.now() - start < timeoutMs) {
      try {
        const resp = await fetch(`${baseUrl}/txs/${txHash}`, {
          headers: { project_id: projectId },
        });

        if (resp.ok) {
          return true;
        }
        // if status is 404 or tx.block is falsy, keep polling
      } catch {
        // on network or fetch error, just retry
      }

      // wait before next poll
      await new Promise((res) => setTimeout(res, pollIntervalMs));
    }

    return false;
  }
}
