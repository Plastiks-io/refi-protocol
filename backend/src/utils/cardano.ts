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

      Decimal.set({ precision: 50, rounding: Decimal.ROUND_HALF_UP });

      // --- Find the roadmap UTXO ---
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

      const newSoldBigInt =
        BigInt(old.soldPlasticCredits) + BigInt(soldPlasticCredit);

      // Use Decimal for precise % progress math
      const newSoldDecimal = new Decimal(newSoldBigInt.toString());
      const totalCreditsDecimal = new Decimal(
        old.totalPlasticCredits.toString()
      );
      const totalTokensDecimal = new Decimal(old.totalPlasticTokens.toString());
      const totalPlasticDecimal = new Decimal(old.totalPlastic.toString());

      const progressPercentageDecimal = newSoldDecimal.div(totalCreditsDecimal);

      // Store basis points as integer for blockchain
      const progressBasisPointsDecimal = progressPercentageDecimal.mul(10000);
      const progressBigInt = BigInt(progressBasisPointsDecimal.toFixed(0));

      // Calculate sent/recovered using precise percentage
      const newSentBigInt = BigInt(
        progressPercentageDecimal.mul(totalTokensDecimal).toFixed(0)
      );
      const recoveredBigInt = BigInt(
        progressPercentageDecimal.mul(totalPlasticDecimal).toFixed(0)
      );

      // --- Updated roadmap datum ---
      const updatedDatum = new Constr(0, [
        fromText(old.preId),
        fromText(old.roadmapId),
        fromText(old.roadmapName),
        fromText(old.roadmapDescription),
        progressBigInt,
        old.adminsPkh,
        old.prePkh,
        old.preSkh,
        BigInt(old.totalPlasticCredits),
        newSoldBigInt,
        BigInt(old.totalPlasticTokens),
        newSentBigInt,
        BigInt(old.totalPlastic),
        recoveredBigInt,
        fromText(old.createdAt),
      ]);

      // --- Stake reward UTXO ---
      const stakeUtxos = await lucid.utxosAt(this.stakeRewardAddress);
      if (stakeUtxos.length === 0) throw new Error("No stake UTxOs found");
      const stakeUtxo = stakeUtxos[0];
      if (!stakeUtxo.datum) throw new Error("Missing stake datum");
      const lender = parseLenderDatum(Data.from(stakeUtxo.datum));

      const adminPkh = await getPubKeyHash(lucid);
      if (!lender.adminsPkh.includes(adminPkh)) throw new Error("Unauthorized");

      const ptUnit = `${this.policyId}${this.ptAssetName}`;
      const needPT = BigInt(soldPlasticCredit) * 80n;
      const contractPT = BigInt(stakeUtxo.assets[ptUnit] || 0n);

      // Collect from admin if not enough PT
      let extraAdminUtxo;
      if (contractPT < needPT) {
        const adminUtxos = await lucid.utxosAt(adminAddress);
        extraAdminUtxo = adminUtxos.find(
          (u) => BigInt(u.assets[ptUnit] || 0n) >= needPT - contractPT
        );
        if (!extraAdminUtxo) throw new Error("Admin has insufficient PT");
      }

      // Redeemers
      const refiRedeemer = Data.to(new Constr(0, [progressBigInt]));
      const lenderRedeemer = Data.to(
        buildLenderAction({ type: "FundPlastikToEscrow", amount: needPT })
      );

      // --- Reward calculation ---
      const rewardMicroBigInt = BigInt(
        new Decimal(soldPlasticCredit)
          .mul(new Decimal("1000000"))
          .mul(new Decimal("0.02")) // 2%
          .toFixed(0)
      );

      // --- Update lenders list ---
      const totalPTDecimal = new Decimal(lender.totalPT.toString());
      const updatedLenders = lender.lenders.map(
        ([pk, [bal, oldDebt]]): [string, [bigint, bigint]] => {
          const balBig = BigInt(bal);
          const oldDebtBig = BigInt(oldDebt);
          if (totalPTDecimal.isZero()) {
            return [pk, [balBig, oldDebtBig]];
          }
          const balDecimal = new Decimal(balBig.toString());
          const shareBigInt = BigInt(
            balDecimal
              .div(totalPTDecimal)
              .mul(new Decimal(rewardMicroBigInt.toString()))
              .toFixed(0)
          );
          return [pk, [balBig, oldDebtBig + shareBigInt]];
        }
      );

      // --- New lender datum ---
      const newLenderDatum: LenderDatum = {
        adminsPkh: lender.adminsPkh,
        totalPT: lender.totalPT,
        totalReward: lender.totalReward + rewardMicroBigInt,
        lenders: updatedLenders,
      };

      // --- Assets maps (force all to BigInt) ---
      const usdmAssetUnit: string = `${this.policyId}${this.usdmAssetName}`;
      const refiAssets: Assets = {
        lovelace: BigInt(matchedUtxo.assets.lovelace || 0n),
        [ptUnit]: BigInt(matchedUtxo.assets[ptUnit] || 0n) + needPT,
      };
      if (matchedUtxo.assets[usdmAssetUnit] !== undefined) {
        refiAssets[usdmAssetUnit] = BigInt(
          matchedUtxo.assets[usdmAssetUnit] || 0n
        );
      }

      const stakeAssets: Assets = {
        [usdmAssetUnit]:
          BigInt(stakeUtxo.assets[usdmAssetUnit] || 0n) + rewardMicroBigInt,
      };
      if (BigInt(stakeUtxo.assets[ptUnit] || 0n) - needPT >= 0n) {
        stakeAssets[ptUnit] = BigInt(stakeUtxo.assets[ptUnit] || 0n) - needPT;
      }

      // --- Transaction build ---
      let txBuilder = lucid
        .newTx()
        .collectFrom([matchedUtxo], refiRedeemer)
        .attachSpendingValidator(this.refiValidator)
        .payToContract(
          lucid.utils.validatorToAddress(this.refiValidator),
          { inline: Data.to(updatedDatum) },
          refiAssets
        )
        .collectFrom([stakeUtxo], lenderRedeemer);

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

      // Completion check
      if (progressBigInt === 10000n) {
        const ptAssetUnit = `${this.policyId}${this.ptAssetName}`;
        await Transaction.create({
          txDate: new Date(),
          txFee: 2,
          amount: Number(needPT), // explicit conversion for DB
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

  // public async getRoadmapDatum(
  //   preId: string,
  //   roadmapId: string,
  //   currentProgress?: number,
  //   txHash?: string
  // ): Promise<ProjectDatum> {
  //   const lucid = this.getLucid();
  //   const utxos: UTxO[] = await lucid.utxosAt(this.refiContractAddress);

  //   // check if txHash is updated OnChain and the progress of it is greater than current progress
  //   // 1. Find the UTxO whose datum’s roadmapId field matches
  //   for (const utxo of utxos) {
  //     if (!utxo.datum) continue;

  //     const decoded = Data.from(utxo.datum) as Constr<Data>;
  //     const fields = decoded.fields;

  //     // Field layout (0: preId, 1: roadmapId, 2: name, 3: desc, 4: progress, …)
  //     const thisRoadmapId = toText(fields[1] as string);
  //     if (thisRoadmapId !== roadmapId) continue;

  //     // 2. Extract and convert each field
  //     const rawProgress = Number(fields[4] as bigint);
  //     const progress = rawProgress > 0 ? rawProgress / 100 : 0;

  //     const paymentCred = lucid.utils.keyHashToCredential(fields[6] as string);
  //     const stakeCred = lucid.utils.keyHashToCredential(fields[7] as string);
  //     const preAddress = lucid.utils.credentialToAddress(
  //       paymentCred,
  //       stakeCred
  //     );

  //     const precisionFactor = 1_000_000n; // 1 PC = 1,000,000 micro PC
  //     const ptAssetUnit = `${this.policyId}${this.ptAssetName}`;
  //     const fundsMissing =
  //       ((utxo.assets[ptAssetUnit] ?? 0n) * precisionFactor) / 100n;

  //     const usdmAssetUnit: string = `${this.policyId}${this.usdmAssetName}`;
  //     const fundsDistributed =
  //       utxo.assets[usdmAssetUnit] ?? 0n / precisionFactor;

  //     // 3. Build and return the ProjectDatum
  //     return {
  //       preId: toText(fields[0] as string),
  //       roadmapId: thisRoadmapId,
  //       roadmapName: toText(fields[2] as string),
  //       roadmapDescription: toText(fields[3] as string),
  //       progress,
  //       preAddress,
  //       totalPlasticCredits: Number(fields[8] as bigint),
  //       soldPlasticCredits: Number(fields[9] as bigint),
  //       totalPlasticTokens: Number(fields[10] as bigint),
  //       sentPlasticTokens: Number(fields[11] as bigint),
  //       totalPlastic: Number(fields[12] as bigint),
  //       recoveredPlastic: Number(fields[13] as bigint),
  //       createdAt: toText(fields[14] as string),
  //       status: "active",
  //       fundsMissing: fundsMissing.toString(),
  //       fundsDistributed: fundsDistributed.toString(),
  //     };
  //   }

  //   throw new Error(
  //     `Roadmap ${roadmapId} not found at ${this.refiContractAddress}`
  //   );
  // }
  public async getRoadmapDatum(
    preId: string,
    roadmapId: string,
    txHash?: string,
    currentProgress?: number
  ): Promise<ProjectDatum> {
    const lucid = this.getLucid();

    // If txHash is provided, first confirm the transaction is on-chain
    if (txHash) {
      console.log(
        `Waiting for transaction ${txHash} to be confirmed on-chain...`
      );
      const confirmed = await this.updatedOnChain(txHash);
      if (!confirmed) {
        throw new Error(
          `Transaction ${txHash} not confirmed on-chain within timeout`
        );
      }
      console.log(`Transaction ${txHash} confirmed on-chain`);
    }

    // If currentProgress is provided, we need to poll until progress increases
    const maxRetries = currentProgress !== undefined ? 10 : 1;
    const retryDelay = 5000; // 5 seconds

    for (let attempt = 0; attempt < maxRetries; attempt++) {
      const utxos: UTxO[] = await lucid.utxosAt(this.refiContractAddress);

      // Find the UTxO whose datum's roadmapId field matches
      for (const utxo of utxos) {
        if (!utxo.datum) continue;

        const decoded = Data.from(utxo.datum) as Constr<Data>;
        const fields = decoded.fields;

        // Field layout (0: preId, 1: roadmapId, 2: name, 3: desc, 4: progress, …)
        const thisRoadmapId = toText(fields[1] as string);
        if (thisRoadmapId !== roadmapId) continue;

        // Extract and convert progress field
        const rawProgress = Number(fields[4] as bigint);
        const progress = rawProgress > 0 ? rawProgress / 100 : 0;

        // Check if progress has increased (if currentProgress is provided)
        if (currentProgress !== undefined && progress <= currentProgress) {
          console.log(
            `Progress hasn't increased yet. Current: ${progress}, Expected: >${currentProgress}`
          );

          // If not the last attempt, wait and retry
          if (attempt < maxRetries - 1) {
            await new Promise((resolve) => setTimeout(resolve, retryDelay));
            break; // Break inner loop to retry fetching UTxOs
          } else {
            // Last attempt - could throw error or return current data
            console.log(`Max retries reached. Progress still at ${progress}`);
            // Uncomment the line below if you want to throw an error when progress doesn't increase
            // throw new Error(`Progress did not increase beyond ${currentProgress} after ${maxRetries} attempts`);
          }
        }

        // Build the credential objects
        const paymentCred = lucid.utils.keyHashToCredential(
          fields[6] as string
        );
        const stakeCred = lucid.utils.keyHashToCredential(fields[7] as string);
        const preAddress = lucid.utils.credentialToAddress(
          paymentCred,
          stakeCred
        );

        // Calculate funds
        const precisionFactor = 1_000_000n; // 1 PC = 1,000,000 micro PC
        const ptAssetUnit = `${this.policyId}${this.ptAssetName}`;
        const fundsMissing =
          ((utxo.assets[ptAssetUnit] ?? 0n) * precisionFactor) / 100n;

        const usdmAssetUnit: string = `${this.policyId}${this.usdmAssetName}`;
        const fundsDistributed =
          utxo.assets[usdmAssetUnit] ?? 0n / precisionFactor;

        // Build and return the ProjectDatum
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
    }

    throw new Error(
      `Roadmap ${roadmapId} not found at ${this.refiContractAddress}${
        currentProgress !== undefined
          ? ` or progress did not increase beyond ${currentProgress}`
          : ""
      }`
    );
  }

  public async fundsUpdated(
    preId: string,
    roadmapId: string,
    txHash: string,
    currentUSDM?: number
  ): Promise<ProjectDatum> {
    const lucid = this.getLucid();

    // If txHash is provided, first confirm the transaction is on-chain
    if (txHash) {
      console.log(
        `Waiting for transaction ${txHash} to be confirmed on-chain...`
      );
      const confirmed = await this.updatedOnChain(txHash);
      if (!confirmed) {
        throw new Error(
          `Transaction ${txHash} not confirmed on-chain within timeout`
        );
      }
      console.log(`Transaction ${txHash} confirmed on-chain`);
    }

    const maxRetries = currentUSDM !== undefined ? 10 : 1;
    const retryDelay = 5000; // 5 seconds

    for (let attempt = 0; attempt < maxRetries; attempt++) {
      const utxos: UTxO[] = await lucid.utxosAt(this.refiContractAddress);

      for (const utxo of utxos) {
        if (!utxo.datum) continue;
        const decoded = Data.from(utxo.datum) as Constr<Data>;
        const fields = decoded.fields;
        const thisRoadmapId = toText(fields[1] as string);
        const thisPreId = toText(fields[0] as string);
        if (thisRoadmapId !== roadmapId || thisPreId !== preId) continue;

        const usdmAssetUnit = `${this.policyId}${this.usdmAssetName}`;
        const usdmAmount = Number(utxo.assets[usdmAssetUnit]) || 0;

        // If currentUSDM is provided, check if amount increased
        if (currentUSDM !== undefined && usdmAmount <= currentUSDM) {
          console.log(
            `USDM amount hasn't increased yet. Current: ${usdmAmount}, Expected: >${currentUSDM}`
          );

          // Not updated yet, wait and retry
          continue;
        }

        // Build the credential objects
        const paymentCred = lucid.utils.keyHashToCredential(
          fields[6] as string
        );
        const stakeCred = lucid.utils.keyHashToCredential(fields[7] as string);
        const preAddress = lucid.utils.credentialToAddress(
          paymentCred,
          stakeCred
        );

        // Calculate funds
        const precisionFactor = 1_000_000n; // 1 PC = 1,000,000 micro PC
        const ptAssetUnit = `${this.policyId}${this.ptAssetName}`;
        const fundsMissing =
          ((utxo.assets[ptAssetUnit] ?? 0n) * precisionFactor) / 100n;

        const fundsDistributed =
          utxo.assets[usdmAssetUnit] ?? 0n / precisionFactor;

        return {
          preId: toText(fields[0] as string),
          roadmapId: thisRoadmapId,
          roadmapName: toText(fields[2] as string),
          roadmapDescription: toText(fields[3] as string),
          progress: Number(fields[4] as bigint) / 100,
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

      if (attempt < maxRetries - 1) {
        // Wait for the next polling interval before retrying
        await new Promise((res) => setTimeout(res, retryDelay));
      }
    }

    throw new Error(
      `Roadmap ${roadmapId} not found or funds not updated at ${this.refiContractAddress} after ${maxRetries} attempts`
    );
  }

  public async updatedDataOnChain(
    txHash: string,
    stakedNumber: number,
    rewardNumber: number
  ): Promise<boolean> {
    try {
      const lucid = this.getLucid();

      // If txHash is provided, first confirm the transaction is on-chain
      if (txHash) {
        console.log(
          `Waiting for transaction ${txHash} to be confirmed on-chain...`
        );
        const confirmed = await this.updatedOnChain(txHash);
        if (!confirmed) {
          console.error(
            `Transaction ${txHash} not confirmed on-chain within timeout`
          );
          return false;
        }
        console.log(`Transaction ${txHash} confirmed on-chain`);
      }

      const maxRetries = 10;
      const retryDelay = 5000; // 5 seconds

      for (let attempt = 0; attempt < maxRetries; attempt++) {
        console.log(
          `Checking data update attempt ${attempt + 1}/${maxRetries}...`
        );

        const utxos: UTxO[] = await lucid.utxosAt(this.stakeRewardAddress);

        if (utxos.length === 0) {
          console.log("No UTxOs found at stake reward address");
          if (attempt < maxRetries - 1) {
            await new Promise((res) => setTimeout(res, retryDelay));
            continue;
          }
          console.error(
            "No UTxOs found at stake reward address after all retries"
          );
          return false;
        }

        const stakeRewardUtxo = utxos[0];
        const currentStakedNumber = Number(
          stakeRewardUtxo.assets[`${this.policyId}${this.ptAssetName}`] ?? 0n
        );
        const currentRewardNumber = Number(
          stakeRewardUtxo.assets[`${this.policyId}${this.usdmAssetName}`] ?? 0n
        );

        console.log(
          `Original staked: ${stakedNumber}, Current staked: ${currentStakedNumber}`
        );
        console.log(
          `Original reward: ${rewardNumber}, Current reward: ${currentRewardNumber}`
        );

        // Check if EITHER the staked number OR reward number has changed
        const stakedChanged = currentStakedNumber !== stakedNumber;
        const rewardChanged = currentRewardNumber !== rewardNumber;

        if (stakedChanged || rewardChanged) {
          console.log("✅ Data successfully updated on-chain");
          if (stakedChanged) {
            console.log(
              `  Staked amount changed: ${stakedNumber} → ${currentStakedNumber}`
            );
          }
          if (rewardChanged) {
            console.log(
              `  Reward amount changed: ${rewardNumber} → ${currentRewardNumber}`
            );
          }
          return true; // Data has been updated, return success
        }

        // If this is the last attempt, log error and return false
        if (attempt === maxRetries - 1) {
          console.error(
            `Data not updated after ${maxRetries} attempts. ` +
              `Staked: ${currentStakedNumber} (unchanged from ${stakedNumber}), ` +
              `Reward: ${currentRewardNumber} (unchanged from ${rewardNumber})`
          );
          return false;
        }

        // Wait for the next polling interval before retrying
        console.log(
          `No changes detected yet, waiting ${retryDelay}ms before retry...`
        );
        await new Promise((res) => setTimeout(res, retryDelay));
      }

      // This should never be reached, but just in case
      return false;
    } catch (error) {
      console.error("❌ Worker error:", error);
      return false; // Return false instead of throwing error
    }
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
