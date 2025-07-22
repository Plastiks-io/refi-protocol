import { Roadmap } from "@/redux/roadmapSlice";
import { BrowserWallet } from "@meshsdk/core";
import axios from "axios";
import { Constr, fromText, toText, Validator, WalletApi } from "lucid-cardano";
import { Lucid, Blockfrost, getAddressDetails, Data } from "lucid-cardano";
import type { Assets, Network } from "lucid-cardano";
import { toast } from "sonner";

export interface NetworkConfig {
  projectId: string;
  baseUrl: string;
  lucidNetwork: Network;
}

interface RoadmapDatum {
  preId: string;
  roadmapId: string;
  roadmapName: string;
  roadmapDescription: string;
  progress: bigint;
  adminsPkh: string[]; // Array of admin PKHs
  prePkh: string;
  preSkh: string;
  totalPlasticCredits: bigint;
  soldPlasticCredits: bigint;
  totalPlasticTokens: bigint;
  sentPlasticTokens: bigint;
  totalPlastic: bigint;
  recoverPlastic: bigint;
  createdAt: string;
}

// 1. MeshBrowserWallet is what enable() returns
type MeshBrowserWallet = Awaited<ReturnType<typeof BrowserWallet.enable>>;
// 2. The CIP‑30 API you actually call against
type MeshWalletApi = MeshBrowserWallet["walletInstance"];

type LenderDatum = {
  adminsPkh: string[]; // first field
  totalPT: bigint; // second field
  totalReward: bigint; // third field
  lenders: [string, [bigint, bigint]][]; // fourth field
};

/**
 * A Cardano client that selects network based on wallet network ID.
 * networkId 1 = Mainnet, 0 = Testnet (preprod/preview)
 */
export class Cardano {
  public lucidInstance: Lucid | null = null;
  private config!: NetworkConfig;
  public ptAssetUnit: string = import.meta.env.VITE_PLASTIC_TOKEN!;
  public usdmAssetUnit: string = import.meta.env.VITE_USDM_TOKEN!;
  public pcAssetUnit: string = import.meta.env.VITE_PC_TOKEN!;
  // Define supported network configs
  private static NETWORKS: Record<number, NetworkConfig> = {
    1: {
      baseUrl: import.meta.env.VITE_BLOCKFROST_MAINNET_URL,
      projectId: import.meta.env.VITE_BLOCKFROST_MAINNET_PROJECT_ID,
      lucidNetwork: "Mainnet",
    },
    0: {
      baseUrl: import.meta.env.VITE_BLOCKFROST_URL,
      projectId: import.meta.env.VITE_BLOCKFROST_PROJECT_ID,
      lucidNetwork: "Preprod",
    },
  };

  /**
   * Initialize Lucid based on the wallet's network ID.
   * @param networkId 0 for Testnet, 1 for Mainnet
   */
  public async init(networkId: number): Promise<void> {
    const cfg = Cardano.NETWORKS[networkId];
    if (!cfg) {
      throw new Error(
        `Unsupported network ID ${networkId}. Expected 0 (Testnet) or 1 (Mainnet).`
      );
    }
    this.config = cfg;

    if (!this.lucidInstance) {
      const { projectId, baseUrl, lucidNetwork } = this.config;
      if (!projectId) {
        throw new Error(
          "Missing Blockfrost project ID in environment for network ID " +
            networkId
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

  public meshToLucidAdapter(meshApi: MeshWalletApi): WalletApi {
    return {
      getNetworkId: () => meshApi.getNetworkId(),
      getUtxos: () => meshApi.getUtxos(),
      getBalance: () => meshApi.getBalance(),
      getUsedAddresses: () => meshApi.getUsedAddresses(),
      getUnusedAddresses: () => meshApi.getUnusedAddresses(),
      getChangeAddress: () => meshApi.getChangeAddress(),
      getRewardAddresses: () => meshApi.getRewardAddresses(),

      // normalize Mesh’s optional-return into a concrete array
      getCollateral: () => meshApi.getCollateral().then((u) => u ?? []),

      signTx: (tx, partial) => meshApi.signTx(tx, partial),
      submitTx: (tx) => meshApi.submitTx(tx),

      // Lucid calls (address, payload)
      // Mesh expects (payload, address)
      // and returns { signature, key, … }
      signData: async (address, payload) => {
        const ds = await meshApi.signData(payload, address);
        return { signature: ds.signature, key: ds.key };
      },

      // Mesh only has getCollateral under experimental
      experimental: {
        getCollateral: () =>
          meshApi.experimental.getCollateral().then((u) => u ?? []),
        // stub out event methods so Lucid’s types are satisfied
        on: (_event, _cb) => {
          /* no-op */
        },
        off: (_event, _cb) => {
          /* no-op */
        },
      },
    };
  }
  public stakeRewardValidator: Validator = {
    type: "PlutusV2",
    script: import.meta.env.VITE_STAKE_REWARD_CBOR!,
  };

  public refiValidator: Validator = {
    type: "PlutusV2",
    script: import.meta.env.VITE_REFI_CBOR!,
  };

  public stakeRewardAddress = () => {
    if (!this.lucidInstance) throw new Error("Lucid not initialized");
    return this.lucidInstance.utils.validatorToAddress(
      this.stakeRewardValidator
    );
  };

  public refiContractAddress = () => {
    if (!this.lucidInstance) throw new Error("Lucid not initialized");
    return this.lucidInstance.utils.validatorToAddress(this.refiValidator);
  };

  public depositPlastik = async (
    wallet: BrowserWallet,
    depositAmount: bigint
  ): Promise<string> => {
    try {
      console.log("quantity", depositAmount);
      const walletApi = this.meshToLucidAdapter(wallet.walletInstance);
      // initialize lucid if not initialized
      if (!this.lucidInstance) {
        const networkId = await walletApi.getNetworkId();
        await this.init(networkId);
      }
      const lucid = this.getLucid();
      lucid.selectWallet(walletApi);
      const stakeRewardAddress = this.stakeRewardAddress();
      const stakeRewardUTXOs = await lucid.utxosAt(stakeRewardAddress);
      const userAddress = await lucid.wallet.address();
      const pkh = await this.getPubKeyHash(lucid);
      // 6.3) If no UTxO ⇒ first deposit ever, we must initialize the datum with (adminPkh, totalPT, totalReward, lenders)
      if (stakeRewardUTXOs.length === 0) {
        const initialDatum: LenderDatum = {
          adminsPkh: [pkh],
          totalPT: depositAmount,
          totalReward: 0n,
          lenders: [[pkh, [depositAmount, 0n]]],
        };

        const tx = await lucid
          .newTx()
          .payToContract(
            stakeRewardAddress,
            { inline: Data.to(this.buildLenderDatum(initialDatum)) },
            {
              // lock exactly depositAmount PLASTIK tokens into the script
              [this.ptAssetUnit]: depositAmount,
            }
          )
          .complete();

        const signed = await tx.sign().complete();
        const txHash = await signed.submit();
        return txHash;
      }

      // 6.4) Otherwise, there is already a datum. We’ll pull the first UTxO (assume singular for simplicity)
      const stakeRewardUTXO = stakeRewardUTXOs[0];
      if (!stakeRewardUTXO.datum) {
        throw new Error("Missing datum in contract UTxO");
      }
      console.log("Stake Reward UTXO:", stakeRewardUTXOs);

      console.log("Working upto here", stakeRewardUTXO.datum);

      // 6.5) Parse the existing on‐chain datum
      const currentDatum = this.parseLenderDatum(
        Data.from(stakeRewardUTXO.datum)
      );

      // 6.6) Build “updatedLenders” array
      let lenderExists = false;
      const updatedLenders = currentDatum.lenders.map(
        ([pubKey, [balance, rewardDebt]]): [string, [bigint, bigint]] => {
          console.log(pubKey + "|| " + pkh);

          if (pubKey === pkh) {
            // 1) New balance = old + deposit
            const newBalance = balance + depositAmount;
            lenderExists = true;
            return [pubKey, [newBalance, rewardDebt]];
          }
          return [pubKey, [balance, rewardDebt]];
        }
      );

      // If the depositor was not in the list, append them
      if (!lenderExists) {
        updatedLenders.push([pkh, [depositAmount, 0n]]);
      }

      // 6.7) Build the new higher‐level datum (keeping adminPkh unchanged)
      const newDatum: LenderDatum = {
        adminsPkh: currentDatum.adminsPkh,
        totalPT: currentDatum.totalPT + depositAmount,
        totalReward: currentDatum.totalReward,
        lenders: updatedLenders,
      };

      // 6.8) Find a UTxO in the user’s wallet holding at least depositAmount PLASTIK
      const userUtxos = await lucid.wallet.getUtxos();
      const userUtxo = userUtxos.find(
        (utxo) =>
          utxo.assets[this.ptAssetUnit] &&
          utxo.assets[this.ptAssetUnit] >= depositAmount
      );
      if (!userUtxo) {
        throw new Error("User has insufficient PLASTIK tokens");
      }

      // 6.9) Build a Redeemer = `Deposit` (Constr(0, []))
      const redeemer = this.buildLenderAction({ type: "Deposit" });

      // 6.10) Construct the new Tx:
      const tx = await lucid
        .newTx()
        .collectFrom([stakeRewardUTXO], Data.to(redeemer))
        .collectFrom([userUtxo])
        .payToContract(
          stakeRewardAddress,
          { inline: Data.to(this.buildLenderDatum(newDatum)) },
          {
            ...stakeRewardUTXO.assets,
            [this.ptAssetUnit]:
              (stakeRewardUTXO.assets[this.ptAssetUnit] || 0n) + depositAmount,
          }
        )
        .attachWithdrawalValidator(this.stakeRewardValidator)
        .addSigner(userAddress)
        .complete();

      const signed = await tx.sign().complete();
      const txHash = await signed.submit();
      return txHash;
    } catch (error) {
      console.error("Error Depositing Plastik Token:", error);
      throw new Error("Failed to deposite plastik token");
    }
  };

  public withdrawPlastik = async (
    wallet: BrowserWallet,
    withdrawAmount: bigint
  ): Promise<string> => {
    try {
      const walletApi = this.meshToLucidAdapter(wallet.walletInstance);
      // initialize lucid if not initialized
      if (!this.lucidInstance) {
        const networkId = await walletApi.getNetworkId();
        await this.init(networkId);
      }
      const lucid = this.getLucid();
      lucid.selectWallet(walletApi);
      // Get the wallet's public key hash
      const pkh = await this.getPubKeyHash(lucid);

      // Fetch existing UTxO at the stake reward address
      const stakeRewardAddress = this.stakeRewardAddress();
      const stakeRewardUTXOs = await lucid.utxosAt(stakeRewardAddress);
      const userAddress = await lucid.wallet.address();

      // Check if contract has any UTxOs
      if (stakeRewardUTXOs.length === 0) {
        throw new Error("No contract UTxOs found - nothing to withdraw");
      }

      // Get the first contract UTxO (assuming single UTxO for simplicity)
      const stakeRewardUTXO = stakeRewardUTXOs[0];
      if (!stakeRewardUTXO.datum) {
        throw new Error("Missing datum in contract UTxO");
      }

      // Parse the existing on-chain datum
      const currentDatum = this.parseLenderDatum(
        Data.from(stakeRewardUTXO.datum)
      );

      // Find the user in the lenders list
      const userLenderIndex = currentDatum.lenders.findIndex(
        ([pubKey, _]) => pubKey === pkh
      );

      if (userLenderIndex === -1) {
        throw new Error("User is not a lender in this contract");
      }

      const [_, [currentBalance, currentRewardDebt]] =
        currentDatum.lenders[userLenderIndex];

      // Check if user has sufficient balance
      if (currentBalance < withdrawAmount) {
        throw new Error(
          `Insufficient balance. Available: ${currentBalance}, Requested: ${withdrawAmount}`
        );
      }

      // Build updated lenders array
      const updatedLenders: [string, [bigint, bigint]][] = currentDatum.lenders
        .map(
          (
            [pubKey, [balance, rewardDebt]],
            index
          ): [string, [bigint, bigint]] => {
            if (index === userLenderIndex) {
              const newBalance = balance - withdrawAmount;
              return [pubKey, [newBalance, rewardDebt]];
            }
            return [pubKey, [balance, rewardDebt]];
          }
        )
        .filter(([_, [balance, __]]) => balance > 0n); // Remove lenders with 0 balance

      let pendingReward = 0n; // Initialize pending reward
      // If user id withdrawing the full stake then we can also pay any pending rewards
      if (currentBalance === withdrawAmount) {
        // find the pending reward for the user from their pkh
        pendingReward =
          currentDatum.lenders.find((lender) => lender[0] === pkh)?.[1][1] ||
          0n;
      }
      // Build the new datum
      const newDatum: LenderDatum = {
        adminsPkh: currentDatum.adminsPkh,
        totalPT: currentDatum.totalPT - withdrawAmount,
        totalReward: currentDatum.totalReward - pendingReward,
        lenders: updatedLenders,
      };

      // Check if contract has enough PLASTIK tokens to withdraw
      const contractPtBalance = stakeRewardUTXO.assets[this.ptAssetUnit] || 0n;
      if (contractPtBalance < withdrawAmount) {
        throw new Error(
          "Contract has insufficient PLASTIK tokens to withdraw because your assets are being funded to the roadmap"
        );
      }

      // Build the redeemer for Withdraw action
      const redeemer = this.buildLenderAction({
        type: "Withdraw",
        amount: withdrawAmount,
      });

      // Calculate remaining assets in the contract after withdrawal
      const remainingAssets = { ...stakeRewardUTXO.assets };
      remainingAssets[this.ptAssetUnit] = contractPtBalance - withdrawAmount;

      // Build the transaction
      const tx = lucid
        .newTx()
        .collectFrom([stakeRewardUTXO], Data.to(redeemer))
        .attachSpendingValidator(this.stakeRewardValidator)
        .addSigner(userAddress);

      // If there are still lenders or remaining PT, pay back to contract
      if (newDatum.totalPT > 0n && newDatum.lenders.length > 0) {
        tx.payToContract(
          stakeRewardAddress,
          { inline: Data.to(this.buildLenderDatum(newDatum)) },
          remainingAssets
        );
      }

      // Pay withdrawn PLASTIK tokens to user
      tx.payToAddress(userAddress, {
        [this.ptAssetUnit]: withdrawAmount,
      });

      // Pay any pending USDM rewards to user (if any)
      if (pendingReward > 0n) {
        const contractUsdmBalance =
          stakeRewardUTXO.assets[this.usdmAssetUnit] || 0n;

        if (contractUsdmBalance >= pendingReward) {
          tx.payToAddress(userAddress, {
            [this.usdmAssetUnit]: pendingReward,
          });

          // Subtract rewards from remaining contract assets
          remainingAssets[this.usdmAssetUnit] =
            contractUsdmBalance - pendingReward;
        }
      }
      const txComplete = await tx.complete();
      const signed = await txComplete.sign().complete();
      const txHash = await signed.submit();
      return txHash;
    } catch (error: Error | any) {
      console.error("Error initializing Lucid:", error);
      toast.error("Failed to Withdraw plastik token " + error.message, {
        closeButton: true,
      });
      throw new Error("Failed to initialize Lucid for withdrawal");
    }
  };

  public redeemReward = async (wallet: BrowserWallet): Promise<string> => {
    try {
      const walletApi = this.meshToLucidAdapter(wallet.walletInstance);
      // initialize lucid if not initialized
      if (!this.lucidInstance) {
        const networkId = await walletApi.getNetworkId();
        await this.init(networkId);
      }
      const lucid = this.getLucid();
      lucid.selectWallet(walletApi);
      // Get the wallet's public key hash
      const pkh = await this.getPubKeyHash(lucid);
      // Fetch existing UTxO at the stake reward address
      const stakeRewardAddress = this.stakeRewardAddress();
      const stakeRewardUTXOs = await lucid.utxosAt(stakeRewardAddress);
      const userAddress = await lucid.wallet.address();
      if (stakeRewardUTXOs.length === 0) {
        throw new Error("No contract UTxOs found - nothing to redeem");
      }
      const stakeRewardUTXO = stakeRewardUTXOs[0];
      if (!stakeRewardUTXO.datum) {
        throw new Error("Missing datum in contract UTxO");
      }
      const currentDatum = this.parseLenderDatum(
        Data.from(stakeRewardUTXO.datum)
      );
      const userLenderIndex = currentDatum.lenders.findIndex(
        ([pubKey, _]) => pubKey === pkh
      );
      if (userLenderIndex === -1) {
        throw new Error("User is not a lender in this contract");
      }
      const [_, [currentBalance, currentRewardDebt]] =
        currentDatum.lenders[userLenderIndex];
      if (currentRewardDebt === 0n) {
        throw new Error("No rewards to redeem");
      }

      // Build updated lenders array
      const updatedLenders: [string, [bigint, bigint]][] =
        currentDatum.lenders.map(
          (
            [pubKey, [balance, rewardDebt]],
            index
          ): [string, [bigint, bigint]] => {
            if (index === userLenderIndex) {
              // Reset the rewardDebt to 0 after redeeming
              return [pubKey, [balance, 0n]];
            }
            return [pubKey, [balance, rewardDebt]];
          }
        );
      // Build the new datum
      const newDatum: LenderDatum = {
        adminsPkh: currentDatum.adminsPkh,
        totalPT: currentDatum.totalPT,
        totalReward: currentDatum.totalReward - currentRewardDebt,
        lenders: updatedLenders,
      };
      // Build the redeemer for Redeem action
      const redeemer = this.buildLenderAction({ type: "Redeem" });
      // Calculate remaining assets in the contract after redeeming
      const remainingAssets = { ...stakeRewardUTXO.assets };
      console.log("working upto here", typeof currentRewardDebt);
      // if the remaining usdm is greater than 0 then only add it to remainingAssets
      const usdmBalance = remainingAssets[this.usdmAssetUnit] ?? 0n;
      if (usdmBalance - currentRewardDebt > 0n) {
        remainingAssets[this.usdmAssetUnit] = usdmBalance - currentRewardDebt;
      }
      // Build the transaction
      console.log("working upto here!!");
      const tx = await lucid
        .newTx()
        .collectFrom([stakeRewardUTXO], Data.to(redeemer))
        .attachSpendingValidator(this.stakeRewardValidator)
        .payToContract(
          stakeRewardAddress,
          { inline: Data.to(this.buildLenderDatum(newDatum)) },
          remainingAssets
        )
        .payToAddress(userAddress, {
          [this.usdmAssetUnit]: currentRewardDebt,
        })
        .complete();

      const signed = await tx.sign().complete();
      const txHash = await signed.submit();
      return txHash;
    } catch (error) {
      console.error("Error redeeming reward:", error);
      throw new Error(
        error instanceof Error ? error.message : "Failed to redeem reward"
      );
    }
  };

  public fundUSDM = async (
    wallet: BrowserWallet,
    preId: string,
    roadmapId: string,
    USDMAmount: bigint
  ): Promise<string> => {
    try {
      const walletApi = this.meshToLucidAdapter(wallet.walletInstance);
      // initialize lucid if not initialized
      if (!this.lucidInstance) {
        const networkId = await walletApi.getNetworkId();
        await this.init(networkId);
      }
      const lucid = this.getLucid();
      lucid.selectWallet(walletApi);
      // Get the wallet's public key hash
      const adminPkh = await this.getPubKeyHash(lucid);

      // Refi Contract UTxO
      const refiContractAddress = this.refiContractAddress();
      const refiUtxos = await lucid.utxosAt(refiContractAddress);
      const matchedUtxo = refiUtxos.find((utxo) => {
        if (!utxo.datum) return false;
        const datum = Data.from(utxo.datum) as Constr<Data>;
        return (
          toText(datum.fields[0] as string) === preId &&
          toText(datum.fields[1] as string) === roadmapId
        );
      });
      if (!matchedUtxo?.datum) {
        throw new Error(
          `No matching roadmap found for preId: ${preId}, roadmapId: ${roadmapId}`
        );
      }

      const currentPT = matchedUtxo.assets[this.ptAssetUnit] || 0n;
      const sendPlastikToken = (USDMAmount * 100n) / 1_000_000n;
      const remainingPlastikToken = currentPT - sendPlastikToken;

      console.log("USDMAmount:", USDMAmount);
      console.log("Current PT balance in UTxO:", currentPT);
      console.log("Send Plastik Token:", sendPlastikToken);
      console.log("Remaining Plastik Token:", remainingPlastikToken);

      const refiAssets: Assets = {
        lovelace: matchedUtxo.assets.lovelace,
        [this.usdmAssetUnit]:
          (matchedUtxo.assets[this.usdmAssetUnit] || 0n) + USDMAmount,
      };
      if (
        (matchedUtxo.assets[this.ptAssetUnit] || 0n) - sendPlastikToken >
        0n
      ) {
        refiAssets[this.ptAssetUnit] =
          (matchedUtxo.assets[this.ptAssetUnit] || 0n) - sendPlastikToken;
      }

      console.log("refiAssets", refiAssets);

      const refiRedeemer = new Constr(3, []);

      // =----------------------------- Lender Staking Reward Contract ---------- =-------------------

      // Fetch existing UTxO at the contract
      const stakeRewardAddress = this.stakeRewardAddress();
      const stakeRewardUTXOs = await lucid.utxosAt(stakeRewardAddress);

      // Check if contract has any UTxOs
      if (stakeRewardUTXOs.length === 0) {
        throw new Error("No contract UTxOs found - nothing to return");
      }

      // Get the first contract UTxO (assuming single UTxO for simplicity)
      const stakeRewardUTXO = stakeRewardUTXOs[0];
      if (!stakeRewardUTXO.datum) {
        throw new Error("Missing datum in contract UTxO");
      }

      // Parse the existing on-chain datum
      const currentDatum = this.parseLenderDatum(
        Data.from(stakeRewardUTXO.datum)
      );

      // Check that only the on-chain adminPkh can call “AdminWithdraw”
      if (!currentDatum.adminsPkh.includes(adminPkh)) {
        throw new Error("Only the admin can fund USDM");
      }

      // Build the redeemer for AdminReturn action
      const contractRedeemer = this.buildLenderAction({
        type: "FundUSDM",
        amount: USDMAmount,
      });

      // Calculate remaining assets in the contract after returning
      const contractAssets: Assets = {
        lovelace: stakeRewardUTXO.assets.lovelace,
        [this.ptAssetUnit]:
          (stakeRewardUTXO.assets[this.ptAssetUnit] || 0n) + sendPlastikToken,
        [this.usdmAssetUnit]: stakeRewardUTXO.assets[this.usdmAssetUnit] || 0n,
      };

      console.log("contractAssets", contractAssets);

      // Build the transaction
      const tx = await lucid
        .newTx()
        .collectFrom([matchedUtxo], Data.to(refiRedeemer))
        .attachSpendingValidator(this.refiValidator) // required for spending
        .payToContract(
          refiContractAddress,
          { inline: matchedUtxo.datum },
          refiAssets
        )
        .collectFrom([stakeRewardUTXO], Data.to(contractRedeemer))
        .attachSpendingValidator(this.stakeRewardValidator)
        .payToContract(
          stakeRewardAddress,
          { inline: stakeRewardUTXO.datum },
          contractAssets
        )
        .addSigner(await lucid.wallet.address())
        .complete();

      const signed = await tx.sign().complete();
      const txHash = await signed.submit();

      // save the plastik token to Lend S.C as well as usdm funding on DB
      await axios.post(
        `${import.meta.env.VITE_SERVER_URL}/transaction/save-multiple`,
        {
          txDate: new Date(),
          txFee: 2,
          plastikAmount: Number(sendPlastikToken),
          USDMAmount: Number(USDMAmount),
          preId,
          roadmapId,
          plastikToken: this.ptAssetUnit,
          usdmToken: this.usdmAssetUnit,
          hash: txHash,
          type1: "tokenReturn",
          type2: "fundTransfer",
        }
      );
      return txHash;
    } catch (error: Error | any) {
      console.error("Error funding USDM:", error);
      toast.error("Failed to send stablecoins " + error.info, {
        closeButton: true,
      });
      throw new Error(error);
    }
  };

  public getPubKeyHash = async (lucid: Lucid): Promise<string> => {
    const address = await lucid.wallet.address();
    const { paymentCredential } = getAddressDetails(address);
    return paymentCredential?.hash || "";
  };

  public buildLenderDatum(datum: LenderDatum): Constr<Data> {
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

  public parseLenderDatum(data: Data): LenderDatum {
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
              if (
                typeof balance === "bigint" &&
                typeof rewardDebt === "bigint"
              ) {
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

  public buildLenderAction = (action: {
    type:
      | "Deposit"
      | "Withdraw"
      | "Redeem"
      | "FundPlastikToEscrow"
      | "FundUSDM";
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

  /**
   * Fetch on‑chain stake & reward for the connected user in one call.
   * @returns An object { staked, rewardDebt } both as bigints.
   */
  public async getStakeAndReward(
    wallet: BrowserWallet
  ): Promise<{ staked: bigint; rewardDebt: bigint }> {
    // 1) Wire up Lucid & wallet
    const walletApi = this.meshToLucidAdapter(wallet.walletInstance);
    if (!this.lucidInstance) {
      const networkId = await walletApi.getNetworkId();
      await this.init(networkId);
    }
    const lucid = this.getLucid();
    lucid.selectWallet(walletApi);

    // 2) Grab your PKH and the contract UTxOs
    const pkh = await this.getPubKeyHash(lucid);
    const scriptAddr = this.stakeRewardAddress();
    const utxos = await lucid.utxosAt(scriptAddr);

    if (utxos.length === 0) {
      // no on‑chain state yet ⇒ nothing staked, no reward
      return { staked: 0n, rewardDebt: 0n };
    }

    // 3) We assume a single UTxO holds the datum
    const { datum } = utxos[0];
    if (!datum) {
      throw new Error("Stake-reward UTxO has no datum!");
    }

    // 4) Parse the LenderDatum and find your entry
    const onChain = this.parseLenderDatum(Data.from(datum));
    const entry = onChain.lenders.find(([addr, _]) => addr === pkh);

    if (!entry) {
      // you’re not in the lenders list
      return { staked: 0n, rewardDebt: 0n };
    }

    const [_, [staked, rewardDebt]] = entry;
    return { staked, rewardDebt };
  }

  public parseRoadmapDatum(data: Data): RoadmapDatum {
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

  public releaseFunds = async (
    wallet: BrowserWallet,
    preId: string,
    roadmapId: string
  ): Promise<string> => {
    try {
      const walletApi = this.meshToLucidAdapter(wallet.walletInstance);
      // initialize lucid if not initialized
      if (!this.lucidInstance) {
        const networkId = await walletApi.getNetworkId();
        await this.init(networkId);
      }
      const lucid = this.getLucid();
      lucid.selectWallet(walletApi);
      const adminAddress = await lucid.wallet.address();
      const RefiAddress = this.refiContractAddress();

      // 2. Find matching UTXO
      const utxos = await lucid.utxosAt(RefiAddress);
      const matchedUtxo = utxos.find((utxo) => {
        if (!utxo.datum) return false;
        const datum = Data.from(utxo.datum) as Constr<Data>;
        return (
          toText(datum.fields[0] as string) === preId &&
          toText(datum.fields[1] as string) === roadmapId
        );
      });

      // console.log("matchedUtxo", matchedUtxo);

      if (!matchedUtxo) {
        throw new Error(
          `No matching roadmap found for preId: ${preId}, roadmapId: ${roadmapId}`
        );
      }

      const redeemer = Data.to(new Constr(1, []));
      // 3. Decode updated datum to find pre Address
      const oldDatum = Data.from(matchedUtxo.datum!) as Constr<Data>;
      const prePkh = oldDatum.fields[6] as string;
      const preSkh = oldDatum.fields[7] as string;
      const prePaymentCredentail = lucid.utils.keyHashToCredential(prePkh);
      const preStakeCredential = lucid.utils.keyHashToCredential(preSkh);
      const preAddress = lucid.utils.credentialToAddress(
        prePaymentCredentail,
        preStakeCredential
      );

      const usdmValue = matchedUtxo.assets[this.usdmAssetUnit] || 0n;
      // console.log("usdmValue", usdmValue);

      const tx = await lucid
        .newTx()
        .collectFrom([matchedUtxo], redeemer)
        .attachSpendingValidator(this.refiValidator) // required for spending
        .addSigner(adminAddress)
        .payToAddress(preAddress, {
          [this.usdmAssetUnit]: usdmValue,
        })
        .complete();

      const signedTx = await tx.sign().complete();
      const txHash = await signedTx.submit();

      // save the usdm token released on DB divide it with 10^6 because it is in micro at datum
      const usdmValueDivided = Number(usdmValue) / 1000000;

      await axios.post(`${import.meta.env.VITE_SERVER_URL}/transaction/save`, {
        txDate: new Date(),
        txFee: 2,
        amount: usdmValueDivided,
        roadmapId: roadmapId,
        assetId: this.usdmAssetUnit,
        hash: txHash,
        type: "usdmReleased",
      });
      return txHash;
    } catch (error) {
      console.error("Error releasing funds:", error);
      throw new Error(
        error instanceof Error ? error.message : "Failed to release funds"
      );
    }
  };

  public archivedRoadmap = async (
    wallet: BrowserWallet,
    preId: string,
    roadmapId: string
  ) => {
    try {
      const walletApi = this.meshToLucidAdapter(wallet.walletInstance);
      // initialize lucid if not initialized
      if (!this.lucidInstance) {
        const networkId = await walletApi.getNetworkId();
        await this.init(networkId);
      }
      const lucid = this.getLucid();
      lucid.selectWallet(walletApi);
      const adminAddress = await lucid.wallet.address();
      const RefiAddress = this.refiContractAddress();

      // 2. Find matching UTXO
      const utxos = await lucid.utxosAt(RefiAddress);
      const matchedUtxo = utxos.find((utxo) => {
        if (!utxo.datum) return false;
        const datum = Data.from(utxo.datum) as Constr<Data>;
        return (
          toText(datum.fields[0] as string) === preId &&
          toText(datum.fields[1] as string) === roadmapId
        );
      });

      if (!matchedUtxo) {
        throw new Error(
          `No matching roadmap found for preId: ${preId}, roadmapId: ${roadmapId}`
        );
      }

      const redeemer = Data.to(new Constr(2, []));

      //4 . Send utxo asset to admin address
      const tx = await lucid
        .newTx()
        .collectFrom([matchedUtxo], redeemer)
        .addSigner(adminAddress)
        .attachSpendingValidator(this.refiValidator) // required for spending
        .payToAddress(adminAddress, matchedUtxo.assets)
        .complete();

      const signedTx = await tx.sign().complete();
      const txHash = await signedTx.submit();

      return txHash;
    } catch (error: Error | any) {
      console.error("Error archiving roadmap:", error);
      toast.error("Failed to Archive Roadmap  " + " " + error.info, {
        closeButton: true,
      });
      throw new Error(
        error instanceof Error ? error.message : "Failed to archive roadmap"
      );
    }
  };

  public restoreRoadmap = async (wallet: BrowserWallet, roadmap: Roadmap) => {
    try {
      const walletApi = this.meshToLucidAdapter(wallet.walletInstance);
      // initialize lucid if not initialized
      if (!this.lucidInstance) {
        const networkId = await walletApi.getNetworkId();
        await this.init(networkId);
      }
      const lucid = this.getLucid();
      lucid.selectWallet(walletApi);
      const adminPkh = await this.getPubKeyHash(lucid);

      const { paymentCredential, stakeCredential } = getAddressDetails(
        roadmap.preAddress
      );
      if (!paymentCredential || !stakeCredential) {
        throw new Error("Invalid preAddress credentials");
      }

      const datumToLock = new Constr(0, [
        fromText(roadmap.preId),
        fromText(roadmap.roadmapId),
        fromText(roadmap.roadmapName),
        fromText(roadmap.roadmapDescription),
        BigInt(roadmap.progress) * 100n,
        [adminPkh],
        paymentCredential.hash,
        stakeCredential.hash,
        BigInt(roadmap.totalPlasticCredits),
        BigInt(roadmap.soldPlasticCredits),
        BigInt(roadmap.totalPlasticTokens),
        BigInt(roadmap.sentPlasticTokens),
        BigInt(roadmap.totalPlastic),
        BigInt(roadmap.recoveredPlastic),
        fromText(roadmap.createdAt),
      ]);

      const RefiAddress = this.refiContractAddress();
      const usdmValue = (roadmap.soldPlasticCredits * 80) / 100;
      console.log(usdmValue, "usdmValue");

      const microUSDM = BigInt(usdmValue * 1_000_000); // Convert to micro USDM
      let assets: Assets = {
        lovelace: 3_000_000n, // Minimum fee for the transaction
      };
      if (microUSDM > 0n) {
        assets[this.usdmAssetUnit] = microUSDM;
      }

      //4 . lock utxo asset to smart contract
      const tx = await lucid
        .newTx()
        .payToContract(
          RefiAddress,
          {
            inline: Data.to(datumToLock),
          },
          assets
        )
        .complete();
      const signedTx = await tx.sign().complete();
      const txHash = await signedTx.submit();
      return txHash;
    } catch (error) {
      console.error("Error restoring roadmap:", error);
      throw new Error(
        error instanceof Error ? error.message : "Failed to restore roadmap"
      );
    }
  };

  public sentPc = async (
    wallet: BrowserWallet,
    amount: number,
    preId: string,
    roadmapId: string
  ) => {
    try {
      const walletApi = this.meshToLucidAdapter(wallet.walletInstance);

      // Initialize lucid if not initialized
      if (!this.lucidInstance) {
        const networkId = await walletApi.getNetworkId();
        await this.init(networkId);
      }

      const lucid = this.getLucid();
      const adminAddress = import.meta.env.VITE_ADMIN_WALLET_ADDRESS;

      if (!adminAddress || !adminAddress.startsWith("addr")) {
        throw new Error("Invalid or missing admin wallet address.");
      }

      const quantity = amount * 1_000_000; // convert ADA to lovelace

      // Get buyer's address
      lucid.selectWallet(walletApi);

      // Build the atomic transaction
      const tx = await lucid
        .newTx()
        // ADA goes to admin
        .payToAddress(adminAddress, {
          lovelace: BigInt(quantity),
        })
        .complete();

      // Sign and submit the transaction
      const signedTx = await tx.sign().complete();
      const txHash = await signedTx.submit();
      const body = {
        txHash: txHash,
        buyerAddress: await lucid.wallet.address(),
        preId: preId,
        roadmapId: roadmapId,
        soldPlasticCredit: amount,
      };
      const { data: res } = await axios.post(
        `${import.meta.env.VITE_SERVER_URL}/nft/buy`,
        body
      );

      toast.success(res.message, {
        closeButton: true,
      });
    } catch (err: Error | any) {
      console.log("Error sending pc:", err);
      toast.error("Failed to send pc" + " " + err.info, {
        closeButton: true,
      });
      throw new Error(err instanceof Error ? err.message : "Failed to send pc");
    }
  };
}

// create and export a single client instance
export const cardanoClient = new Cardano();
