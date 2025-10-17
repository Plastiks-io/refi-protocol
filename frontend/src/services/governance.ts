import { MeshWalletApi, NetworkConfig } from "@/types/types";
import { BrowserWallet } from "@meshsdk/core";
import axios from "axios";
import {
  Blockfrost,
  Constr,
  Data,
  Lucid,
  Validator,
  WalletApi,
  getAddressDetails,
} from "lucid-cardano";
import { useTransactionToast } from "@/components/CustomToast";
const { showTransactionToast } = useTransactionToast();

class Governance {
  public lucidInstance: Lucid | null = null;
  private isInitializing: boolean = false;
  private initPromise: Promise<void> | null = null;
  private currentNetworkId: number | null = null;

  private config!: NetworkConfig & {
    initialRetirementRate: bigint;
    minVotingPeriod: bigint;
    quorumThreshold: bigint;
  };
  public policyId: string = import.meta.env.VITE_POLICY_ID!;
  public ptAssetName: string = import.meta.env.VITE_PLASTIC_TOKEN_NAME!;

  // Define supported network configs with governance parameters
  private static NETWORKS: Record<
    number,
    NetworkConfig & {
      initialRetirementRate: bigint;
      minVotingPeriod: bigint;
      quorumThreshold: bigint;
    }
  > = {
    1: {
      baseUrl: import.meta.env.VITE_BLOCKFROST_MAINNET_URL,
      projectId: import.meta.env.VITE_BLOCKFROST_MAINNET_PROJECT_ID,
      lucidNetwork: "Mainnet",
      cardanoScanUrl: import.meta.env.VITE_CARDANO_SCAN_MAINNET_URL!,
      initialRetirementRate: 2n,
      minVotingPeriod: 10000n,
      quorumThreshold: 5n,
    },
    0: {
      baseUrl: import.meta.env.VITE_BLOCKFROST_URL,
      projectId: import.meta.env.VITE_BLOCKFROST_PROJECT_ID,
      lucidNetwork: "Preprod",
      cardanoScanUrl: import.meta.env.VITE_CARDANO_SCAN_URL!,
      initialRetirementRate: 2n,
      minVotingPeriod: 10000n,
      quorumThreshold: 5n,
    },
  };

  public governanceValidator: Validator = {
    type: "PlutusV2",
    script: import.meta.env.VITE_GOVERNANCE_CBOR!,
  };

  /**
   * Initialize Lucid based on the wallet's network ID.
   * Prevents multiple simultaneous initializations
   * @param networkId 0 for Testnet, 1 for Mainnet
   */
  public async init(networkId: number): Promise<void> {
    // If already initialized for this network, return immediately
    if (this.lucidInstance && this.currentNetworkId === networkId) {
      return;
    }

    // If initialization is in progress, wait for it
    if (this.isInitializing && this.initPromise) {
      await this.initPromise;
      return;
    }

    // Start new initialization
    this.isInitializing = true;
    this.initPromise = this._initInternal(networkId);

    try {
      await this.initPromise;
    } finally {
      this.isInitializing = false;
      this.initPromise = null;
    }
  }

  private async _initInternal(networkId: number): Promise<void> {
    try {
      const cfg = Governance.NETWORKS[networkId];
      if (!cfg) {
        throw new Error(
          `Unsupported network ID ${networkId}. Expected 0 (Testnet) or 1 (Mainnet).`
        );
      }
      this.config = cfg;

      const { projectId, baseUrl, lucidNetwork } = this.config;
      if (!projectId) {
        throw new Error(
          "Missing Blockfrost project ID in environment for network ID " +
            networkId
        );
      }

      // Only create new instance if needed
      if (!this.lucidInstance || this.currentNetworkId !== networkId) {
        // Add a small delay to ensure WASM is loaded
        await new Promise((resolve) => setTimeout(resolve, 100));

        this.lucidInstance = await Lucid.new(
          new Blockfrost(baseUrl, projectId),
          lucidNetwork
        );
        this.currentNetworkId = networkId;

        console.log(`Lucid initialized for network ${networkId}`);
      }
    } catch (error) {
      console.error("Error in _initInternal:", error);
      // Reset state on error
      this.lucidInstance = null;
      this.currentNetworkId = null;
      throw error;
    }
  }

  /** Ensures Lucid is initialized */
  public getLucid(): Lucid {
    if (!this.lucidInstance) {
      throw new Error("Lucid not initialized: call init(networkId) first.");
    }
    return this.lucidInstance;
  }

  /** Check if Lucid is ready */
  public isReady(): boolean {
    return this.lucidInstance !== null && !this.isInitializing;
  }

  public governanceAddress = (): string => {
    if (!this.lucidInstance) throw new Error("Lucid not initialized");
    return this.lucidInstance.utils.validatorToAddress(
      this.governanceValidator
    );
  };

  public meshToLucidAdapter(meshApi: MeshWalletApi): WalletApi {
    return {
      getNetworkId: () => meshApi.getNetworkId(),
      getUtxos: () => meshApi.getUtxos(),
      getBalance: () => meshApi.getBalance(),
      getUsedAddresses: () => meshApi.getUsedAddresses(),
      getUnusedAddresses: () => meshApi.getUnusedAddresses(),
      getChangeAddress: () => meshApi.getChangeAddress(),
      getRewardAddresses: () => meshApi.getRewardAddresses(),
      getCollateral: () => meshApi.getCollateral().then((u) => u ?? []),
      signTx: (tx, partial) => meshApi.signTx(tx, partial),
      submitTx: (tx) => meshApi.submitTx(tx),
      signData: async (address, payload) => {
        const ds = await meshApi.signData(payload, address);
        return { signature: ds.signature, key: ds.key };
      },
      experimental: {
        getCollateral: () =>
          meshApi.experimental.getCollateral().then((u) => u ?? []),
        on: (_event, _cb) => {
          /* no-op */
        },
        off: (_event, _cb) => {
          /* no-op */
        },
      },
    };
  }

  public getPubKeyHash = async (lucid: Lucid): Promise<string> => {
    const address = await lucid.wallet.address();
    const { paymentCredential } = getAddressDetails(address);
    return paymentCredential?.hash || "";
  };

  // === Data Encoding/Decoding Utilities ===

  public decodeVoteList(listData: Data): Data[] {
    const arr: Data[] = [];
    let cur = listData as Constr<Data>;
    while (cur.index === 1) {
      const [head, tail] = cur.fields as [Data, Data];
      arr.push(head);
      cur = tail as Constr<Data>;
    }
    return arr;
  }

  public encodeVoteList(allVotes: Data[]): Data {
    return allVotes.reduceRight(
      (tail, head) => new Constr(1, [head, tail]),
      new Constr(0, [])
    );
  }

  public voteData(
    voterPkh: string,
    proposalId: bigint,
    inFavor: boolean
  ): Data {
    const boolConstr = new Constr(inFavor ? 1 : 0, []);
    return new Constr(0, [voterPkh, proposalId, boolConstr]);
  }

  public proposalData(
    proposalId: bigint,
    retirementPercentage: bigint,
    proposerPkh: string,
    createdAt: bigint,
    expiresAt: bigint,
    votesFor: bigint,
    votesAgainst: bigint,
    allVotes: Data[],
    executed: boolean
  ): Data {
    const votesList = this.encodeVoteList(allVotes);
    const executedConstr = new Constr(executed ? 1 : 0, []);
    return new Constr(0, [
      proposalId,
      retirementPercentage,
      proposerPkh,
      createdAt,
      expiresAt,
      votesFor,
      votesAgainst,
      votesList,
      executedConstr,
    ]);
  }

  public maybeProposal(p: Data | null): Data {
    return p === null ? new Constr(0, []) : new Constr(1, [p]);
  }

  public governanceDatum(
    currentRetirementRate: bigint,
    activeProp: Data | null,
    proposalCounter: bigint,
    minVotingPeriod: bigint,
    quorumThreshold: bigint
  ): Data {
    return new Constr(0, [
      currentRetirementRate,
      this.maybeProposal(activeProp),
      proposalCounter,
      minVotingPeriod,
      quorumThreshold,
    ]);
  }

  public asPOSIXTime(d: Date | number): bigint {
    return BigInt(d instanceof Date ? d.getTime() : d);
  }

  // === Governance State Management ===

  /**
   * Check if a user has already voted on the active proposal
   * WITH DEBOUNCING to prevent multiple simultaneous calls
   */
  private hasVotedCache = new Map<
    string,
    { result: boolean; timestamp: number }
  >();
  private readonly CACHE_TTL = 5000; // 5 seconds cache

  public async hasUserVoted(
    wallet: BrowserWallet,
    proposalId: bigint
  ): Promise<boolean> {
    try {
      // Check cache first
      const cacheKey = `${proposalId}`;
      const cached = this.hasVotedCache.get(cacheKey);
      if (cached && Date.now() - cached.timestamp < this.CACHE_TTL) {
        return cached.result;
      }

      const walletApi = this.meshToLucidAdapter(wallet.walletInstance);
      const networkId = await walletApi.getNetworkId();

      await this.init(networkId);

      // Double-check lucid is ready
      if (!this.isReady()) {
        console.warn("Lucid not ready, returning false");
        return false;
      }

      const lucid = this.getLucid();
      lucid.selectWallet(walletApi);

      const voterPkh = await this.getPubKeyHash(lucid);
      const utxos = await lucid.utxosAt(this.governanceAddress());

      const govUtxo = utxos.find(
        (u) => u.datum && Data.from(u.datum) instanceof Constr
      );
      if (!govUtxo) {
        this.hasVotedCache.set(cacheKey, {
          result: false,
          timestamp: Date.now(),
        });
        return false;
      }

      const cd = Data.from(govUtxo.datum!) as Constr<Data>;
      const [, maybeProp] = cd.fields as [
        bigint,
        Constr<Data>,
        bigint,
        bigint,
        bigint
      ];

      if (maybeProp.index === 0) {
        this.hasVotedCache.set(cacheKey, {
          result: false,
          timestamp: Date.now(),
        });
        return false;
      }

      const propConstr = maybeProp.fields[0] as Constr<Data>;
      const [propId, , , , , , , allVotesData] = propConstr.fields;

      if (propId !== proposalId) {
        this.hasVotedCache.set(cacheKey, {
          result: false,
          timestamp: Date.now(),
        });
        return false;
      }

      const existingVotes = this.decodeVoteList(allVotesData);

      const result = existingVotes.some((v) => {
        const [voterHash] = (v as Constr<Data>).fields as [
          string,
          bigint,
          Data
        ];
        return voterHash === voterPkh;
      });

      // Cache the result
      this.hasVotedCache.set(cacheKey, { result, timestamp: Date.now() });

      return result;
    } catch (error) {
      console.error("Error checking vote status:", error);
      return false;
    }
  }

  // Clear cache when needed
  public clearVoteCache(): void {
    this.hasVotedCache.clear();
  }

  // 1. Get networkId from wallet
  // 2. await this.init(networkId)
  // 3. Check if ready with this.isReady()
  // 4. Proceed with operation

  public async createProposal(
    wallet: BrowserWallet,
    retirementPercentage: bigint
  ): Promise<string> {
    try {
      const walletApi = this.meshToLucidAdapter(wallet.walletInstance);
      const networkId = await walletApi.getNetworkId();
      await this.init(networkId);

      if (!this.isReady()) {
        throw new Error("Governance client not ready");
      }

      const lucid = this.getLucid();
      lucid.selectWallet(walletApi);

      // createProposal logic
      const proposerPkh = await this.getPubKeyHash(lucid);
      const utxos = await lucid.utxosAt(this.governanceAddress());

      const govUtxo = utxos.find(
        (u) => u.datum && Data.from(u.datum) instanceof Constr
      );
      if (!govUtxo) throw new Error("No governance UTxO at script");

      const cd = Data.from(govUtxo.datum!) as Constr<Data>;
      const [currRate, activeProp, counter, minPeriod, quorum] = cd.fields as [
        bigint,
        Constr<Data>,
        bigint,
        bigint,
        bigint
      ];

      if (activeProp.index === 1)
        throw new Error("Active proposal already exists");

      const now = this.asPOSIXTime(Date.now());
      // 30 days from now
      const expires = now + 30n * 24n * 60n * 60n * 1000n;
      // for testing make it 5 minutes from now
      // const expires = now + 5n * 60n * 1000n;

      const newProp = this.proposalData(
        counter + 1n,
        retirementPercentage,
        proposerPkh,
        now,
        expires,
        0n,
        0n,
        [],
        false
      );

      const newState = this.governanceDatum(
        currRate,
        newProp,
        counter + 1n,
        minPeriod,
        quorum
      );

      const redeemer = Data.to(new Constr(0, [counter + 1n]));

      const tx = await lucid
        .newTx()
        .attachSpendingValidator(this.governanceValidator)
        .collectFrom([govUtxo], redeemer)
        .payToContract(
          this.governanceAddress(),
          { inline: Data.to(newState) },
          govUtxo.assets
        )
        .complete();

      const signed = await tx.sign().complete();
      const txHash = await signed.submit();

      // Clear cache after creating proposal
      this.clearVoteCache();

      await axios.post(
        `${import.meta.env.VITE_SERVER_URL}/transaction/updated`,
        {
          txHash,
        }
      );

      // custom toast
      showTransactionToast({
        title: "Transaction Submitted",
        description: "Your create proposal transaction has been submitted.",
        linkText: "View on CardanoScan",
        linkUrl: `${this.config.cardanoScanUrl}/transaction/${txHash}`,
      });
      return txHash;
    } catch (error) {
      console.error("Error creating proposal:", error);
      throw error;
    }
  }

  public async voteOnProposal(
    inFavor: boolean,
    wallet: BrowserWallet,
    proposalId: bigint
  ): Promise<string> {
    try {
      const walletApi = this.meshToLucidAdapter(wallet.walletInstance);
      const networkId = await walletApi.getNetworkId();
      await this.init(networkId);

      if (!this.isReady()) {
        throw new Error("Governance client not ready");
      }

      const lucid = this.getLucid();
      lucid.selectWallet(walletApi);

      // ... your voteOnProposal logic ...
      const utxos = await lucid.utxosAt(this.governanceAddress());

      const govUtxo = utxos.find(
        (u) => u.datum && Data.from(u.datum) instanceof Constr
      );
      if (!govUtxo) throw new Error("No governance UTxO at script");

      const cd = Data.from(govUtxo.datum!) as Constr<Data>;
      const [currRate, maybeProp, counter, minPeriod, quorum] = cd.fields as [
        bigint,
        Constr<Data>,
        bigint,
        bigint,
        bigint
      ];

      if (maybeProp.index === 0)
        throw new Error("No active proposal to vote on");

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

      const propId = propIdData as bigint;
      const retirementPct = retirementPctData as bigint;
      const proposer = proposerData as string;
      const createdAt = createdAtData as bigint;
      const expiresAt = expiresAtData as bigint;
      const votesFor = votesForData as bigint;
      const votesAgainst = votesAgainstData as bigint;

      if (propId !== proposalId) throw new Error("Proposal ID mismatch");

      const now = this.asPOSIXTime(Date.now());

      if (now < createdAt || now > expiresAt)
        throw new Error("Proposal is not active");

      const voterPkh = await this.getPubKeyHash(lucid);
      const existingVotes = this.decodeVoteList(allVotesData);

      for (const v of existingVotes) {
        const [voterHash] = (v as Constr<Data>).fields as [
          string,
          bigint,
          Data
        ];
        if (voterHash === voterPkh)
          throw new Error("Already voted on this proposal");
      }

      const newVote = this.voteData(voterPkh, proposalId, inFavor);
      const updatedVotesFor = votesFor + (inFavor ? 1n : 0n);
      const updatedVotesAgainst = votesAgainst + (inFavor ? 0n : 1n);
      const updatedAllVotes = this.encodeVoteList([...existingVotes, newVote]);

      const updatedPropConstr = new Constr(0, [
        propId,
        retirementPct,
        proposer,
        createdAt,
        expiresAt,
        updatedVotesFor,
        updatedVotesAgainst,
        updatedAllVotes,
        executedData,
      ]);

      const updatedMaybeProp = new Constr(1, [updatedPropConstr]);

      const newState = new Constr(0, [
        currRate,
        updatedMaybeProp,
        counter,
        minPeriod,
        quorum,
      ]);

      const redeemer = Data.to(new Constr(1, []));

      const tx = await lucid
        .newTx()
        .attachSpendingValidator(this.governanceValidator)
        .collectFrom([govUtxo], redeemer)
        .payToContract(
          this.governanceAddress(),
          { inline: Data.to(newState) },
          govUtxo.assets
        )
        .complete();

      const signedTx = await tx.sign().complete();
      const txHash = await signedTx.submit();

      // custom toast
      showTransactionToast({
        title: "Transaction Submitted",
        description: `Vote ${
          inFavor ? "for" : "against"
        } submitted successfully! Tx: ${txHash}`,
        linkText: "View on CardanoScan",
        linkUrl: `${this.config.cardanoScanUrl}/transaction/${txHash}`,
      });

      // Clear cache after voting
      this.clearVoteCache();

      await axios.post(
        `${import.meta.env.VITE_SERVER_URL}/transaction/updated`,
        {
          txHash,
        }
      );

      return txHash;
    } catch (error) {
      console.error("Error voting:", error);
      throw error;
    }
  }

  public async executeAndUpdateProposal(
    wallet: BrowserWallet,
    proposalId: bigint
  ): Promise<string> {
    try {
      const walletApi = this.meshToLucidAdapter(wallet.walletInstance);
      const networkId = await walletApi.getNetworkId();
      await this.init(networkId);

      if (!this.isReady()) {
        throw new Error("Governance client not ready");
      }

      const lucid = this.getLucid();
      lucid.selectWallet(walletApi);

      // ... your executeAndUpdateProposal logic ...
      const utxos = await lucid.utxosAt(this.governanceAddress());

      const govUtxo = utxos.find(
        (u) => u.datum && Data.from(u.datum) instanceof Constr
      );
      if (!govUtxo) throw new Error("No governance UTxO at script");

      const cd = Data.from(govUtxo.datum!) as Constr<Data>;
      const [currRate, maybeProp, counter, minPeriod, quorum] = cd.fields as [
        bigint,
        Constr<Data>,
        bigint,
        bigint,
        bigint
      ];

      if (maybeProp.index === 0) {
        throw new Error("No active proposal to execute");
      }

      const propConstr = maybeProp.fields[0] as Constr<Data>;
      const [
        propId,
        newRateData,
        ,
        ,
        expiresAt,
        votesFor,
        votesAgainst,
        ,
        executedData,
      ] = propConstr.fields;

      if (propId !== proposalId) {
        throw new Error("Proposal ID mismatch");
      }

      const now = this.asPOSIXTime(Date.now());
      if (now < (expiresAt as bigint)) {
        throw new Error("Proposal still active");
      }

      if ((executedData as Constr<Data>).index === 1) {
        throw new Error("Proposal already executed");
      }

      let finalRate = currRate;
      const vFor = votesFor as bigint;
      const vAgainst = votesAgainst as bigint;

      if (vFor >= quorum && vFor > vAgainst) {
        finalRate = newRateData as bigint;
      }

      const newState = new Constr(0, [
        finalRate,
        new Constr(0, []),
        counter,
        minPeriod,
        quorum,
      ]);

      const redeemer = Data.to(new Constr(2, []));

      const tx = await lucid
        .newTx()
        .attachSpendingValidator(this.governanceValidator)
        .collectFrom([govUtxo], redeemer)
        .payToContract(
          this.governanceAddress(),
          { inline: Data.to(newState) },
          govUtxo.assets
        )
        .complete();

      const signedTx = await tx.sign().complete();
      const txHash = await signedTx.submit();

      // custom toast
      showTransactionToast({
        title: "Transaction Submitted",
        description: `Proposal executed and finalized! Transaction: ${txHash}`,
        linkText: "View on CardanoScan",
        linkUrl: `${this.config.cardanoScanUrl}/transaction/${txHash}`,
      });

      // Clear cache after execution
      this.clearVoteCache();

      await axios.post(
        `${import.meta.env.VITE_SERVER_URL}/transaction/updated`,
        {
          txHash,
        }
      );

      return txHash;
    } catch (error) {
      console.error("Error executing proposal:", error);
      throw error;
    }
  }
}

// Create and export a single client instance
export const governanceClient = new Governance();
