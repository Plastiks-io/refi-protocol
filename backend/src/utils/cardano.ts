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

export interface NetworkConfig {
  projectId: string;
  baseUrl: string;
  lucidNetwork: Network;
}

export class Cardano {
  public lucidInstance: Lucid | null = null;
  public ptAssetUnit: string;
  public usdmAssetUnit: string;
  public pcAssetUnit: string;
  public stakeRewardValidator: Validator;
  public refiValidator: Validator;

  // Define supported network configs
  private static NETWORKS: NetworkConfig = {
    baseUrl: process.env.BLOCKFROST_URL!,
    projectId: process.env.BLOCKFROST_PROJECT_ID!,
    lucidNetwork: "Preprod",
  };

  constructor() {
    // Initialize asset units from environment variables
    this.ptAssetUnit = process.env.PLASTIC_TOKEN!;
    this.usdmAssetUnit = process.env.USDM_TOKEN!;
    this.pcAssetUnit = process.env.PC_ASSET_ID!;

    // Validate required environment variables
    if (!this.ptAssetUnit) {
      throw new Error("PLASTIC_TOKEN environment variable is required");
    }
    if (!this.usdmAssetUnit) {
      throw new Error("USDM_TOKEN environment variable is required");
    }
    if (!this.pcAssetUnit) {
      throw new Error("PC_TOKEN environment variable is required");
    }

    // Initialize validators
    const stakeRewardCbor = process.env.STAKE_REWARD_CBOR;
    const refiCbor = process.env.REFI_CBOR;

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
      const seed = process.env.PC_WALLET!;
      lucid.selectWalletFromSeed(seed);

      const pcAssetId = process.env.PC_ASSET_ID!;

      const tx = await lucid
        .newTx()
        .payToAddress(address, {
          [pcAssetId]: BigInt(amount),
        })
        .complete();

      const txFee = 2;
      const signedTx = await tx.sign().complete();
      const txHash = await signedTx.submit();

      // Create a new transaction in the database
      await Transaction.create({
        txDate: new Date(),
        txFee,
        amount,
        roadmapId,
        assetId: pcAssetId,
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
      const adminSeed = process.env.ADMIN_SEED!;
      lucid.selectWalletFromSeed(adminSeed);
      const adminAddress = await lucid.wallet.address();

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
      const progress = (newSold * 10000n) / old.totalPlasticCredits;
      const newSent = (progress * old.totalPlasticTokens) / 10000n;
      const recovered = (progress * old.totalPlastic) / 10000n;
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

      const ptUnit = process.env.PLASTIC_TOKEN!;
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

      const usdmAssetUnit: string = process.env.USDM_TOKEN!;
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
      if (progress === 10000n) {
        await Transaction.create({
          txDate: new Date(),
          txFee: 2,
          amount: Number(needPT),
          roadmapId,
          assetId: this.pcAssetUnit,
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
    lucid.selectWalletFromSeed(process.env.ADMIN_SEED!);

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

      const precision = 1_000_000n;
      const ptUnit = process.env.PLASTIC_TOKEN!;
      const fundsMissing = (
        ((utxo.assets[ptUnit] ?? 0n) * precision) /
        100n
      ).toString();

      const usdmUnit = process.env.USDM_TOKEN!;
      const fundsDistributed = (
        (utxo.assets[usdmUnit] ?? 0n) / precision
      ).toString();

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
        fundsMissing,
        fundsDistributed,
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
