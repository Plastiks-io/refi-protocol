import axios from "axios";
import { Request, Response } from "express";
import {
  Lucid,
  Blockfrost,
  Script,
  Constr,
  fromText,
  Data,
  toText,
  OutRef,
} from "lucid-cardano";
import dotenv from "dotenv";
import {
  InitializeRoadmapRequest,
  ProjectDatum,
  QueryTransaction,
  ReleaseFundsRequest,
  UpdateRoadmapRequest,
} from "../types/roadmap.types.js";
dotenv.config();
import CompletedRoadmap from "../models/completedRoadmap.model.js";

const initializeLucid = async () => {
  const lucid = await Lucid.new(
    new Blockfrost(
      process.env.BLOCKFROST_URL!,
      process.env.BLOCKFROST_PROJECT_ID!
    ),
    "Preprod"
  );
  return lucid;
};

const bigIntReplacer = (key: string, value: any) => {
  if (typeof value === "bigint") {
    return value.toString();
  }
  if (value instanceof Map) {
    return Object.fromEntries(value);
  }
  return value;
};

const initializeRoadmap = async (
  req: Request,
  res: Response
): Promise<void> => {
  try {
    const {
      preId,
      roadmapId,
      roadmapName,
      roadmapDescription,
      prePkh,
      preSkh,
      totalPlasticCredits,
      totalPlasticTokens,
      totalPlastic,
    }: InitializeRoadmapRequest = req.body;

    if (
      !preId ||
      !roadmapId ||
      !roadmapName ||
      !roadmapDescription ||
      !prePkh ||
      !preSkh ||
      !totalPlasticCredits ||
      !totalPlasticTokens ||
      !totalPlastic
    ) {
      res.status(400).json({ error: "Missing required fields" });
      return;
    }

    const lucid = await initializeLucid();

    const adminSeed: string = process.env.ADMIN_SEED || "";
    lucid.selectWalletFromSeed(adminSeed);
    const { paymentCredential } = lucid.utils.getAddressDetails(
      await lucid.wallet.address()
    );
    const adminPkh = paymentCredential?.hash;

    const cbor = process.env.CBOR!;
    const RefiScript: Script = {
      type: "PlutusV2",
      script: cbor,
    };

    const RefiAddress = lucid.utils.validatorToAddress(RefiScript);

    // check if roadmap already exist with same preId and roadmapId
    const utxos = await lucid.utxosAt(RefiAddress);
    const matchedUtxo = utxos.find((utxo) => {
      if (!utxo.datum) return false;
      const datum = Data.from(utxo.datum) as Constr<Data>;
      return (
        toText(datum.fields[0] as string) === preId &&
        toText(datum.fields[1] as string) === roadmapId
      );
    });

    // check if a roadmap already exist with same preId and roadmapId in completed roadmap
    const completedRoadmap = await CompletedRoadmap.findOne({
      where: {
        preId: preId,
        roadmapId: roadmapId,
      },
    });
    if (completedRoadmap) {
      res.status(409).json({
        message: "Roadmap already exists in completed roadmaps",
        success: false,
      });
      return;
    }

    // 409 Conflict
    if (matchedUtxo) {
      res.status(409).json({
        message: "Roadmap already exists in smart contract",
        success: false,
      });
      return;
    }

    const datumToLock = new Constr(0, [
      fromText(preId || ""),
      fromText(roadmapId || ""),
      fromText(roadmapName || ""),
      fromText(roadmapDescription || ""),
      BigInt(0),
      adminPkh || "",
      prePkh || "",
      preSkh || "",
      BigInt(totalPlasticCredits || 0),
      BigInt(0),
      BigInt(totalPlasticTokens || 0),
      BigInt(0),
      BigInt(totalPlastic || 0),
      BigInt(0),
    ]);

    // console.log("Datum to lock:", datumToLock);
    // console.log("Refi Address:", RefiAddress);
    const AMOUNT = 3_000_000; // 3 ADA
    const tx = await lucid
      .newTx()
      .payToContract(
        RefiAddress,
        {
          inline: Data.to(datumToLock),
        },
        { lovelace: BigInt(AMOUNT) }
      )
      .complete();

    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();

    res.status(200).json({
      message: "Roadmap Initiated successfully",
      txHash,
      success: true,
    });
    return;
  } catch (err) {
    console.error(err);
    res.status(500).json({
      message: "Error occured during the creation of roadmap",
      success: false,
      error: err,
    });
    return;
  }
};

const updateRoadmap = async (req: Request, res: Response): Promise<void> => {
  try {
    const { preId, roadmapId, soldPlasticCredit }: UpdateRoadmapRequest =
      req.body;
    const lucid = await initializeLucid();
    const adminSeed = process.env.ADMIN_SEED!;
    lucid.selectWalletFromSeed(adminSeed);
    const adminAddress = await lucid.wallet.address();
    // 1. Initialize wallet and scripts
    const RefiScript: Script = {
      type: "PlutusV2",
      script: process.env.CBOR!,
    };
    const RefiAddress = lucid.utils.validatorToAddress(RefiScript);

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
      res.status(404).json({ error: "Roadmap UTXO not found" });
      return;
    }

    // 3. Decode and prepare updated datum
    const oldDatum = Data.from(matchedUtxo.datum!) as Constr<Data>;
    const totalPlasticCredits = oldDatum.fields[8] as bigint;
    const newSoldCredits =
      (oldDatum.fields[9] as bigint) + BigInt(soldPlasticCredit);

    // Calculate progress using integer math
    const progress = (newSoldCredits * 10000n) / totalPlasticCredits;
    const newSentTokens = (progress * (oldDatum.fields[10] as bigint)) / 10000n;
    const recoveredPlastic =
      (progress * (oldDatum.fields[12] as bigint)) / 10000n;

    // 4. Build new datum
    const updatedDatum = new Constr(0, [
      oldDatum.fields[0], // preId
      oldDatum.fields[1], // roadmapId
      oldDatum.fields[2], // roadmapName
      oldDatum.fields[3], // roadmapDescription
      progress, // updated progress
      oldDatum.fields[5], // adminPkh
      oldDatum.fields[6], // prePkh
      oldDatum.fields[7], // preSkh
      oldDatum.fields[8], // totalPlasticCredits
      newSoldCredits, // updated sold credits
      oldDatum.fields[10], // totalPlasticTokens
      newSentTokens, // updated sent tokens
      oldDatum.fields[12], // totalPlastic
      recoveredPlastic, // updated recovered plastic
    ]);

    const redeemer = Data.to(new Constr(0, [progress]));
    const plastikAssetId: string = process.env.PLASTIK || "";
    const plastikToLock = (newSentTokens * 80n) / 100n;

    // 5. Build transaction
    // 1 Plastik Credits = 100 Plastik Token
    // lock the  80% plastik token equal to sent plastik token on smart contract
    const tx = await lucid
      .newTx()
      .collectFrom([matchedUtxo], redeemer)
      .attachSpendingValidator(RefiScript) // required for spending
      .addSigner(adminAddress)
      .payToContract(
        RefiAddress,
        { inline: Data.to(updatedDatum) },
        {
          lovelace: matchedUtxo.assets.lovelace,
          [plastikAssetId]: plastikToLock,
        }
      )
      .complete();
    // 6. Sign and submit
    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();

    res.status(200).json({
      message: "Roadmap updated successfully",
      txHash,
      newProgress: Number(progress) / 100,
      success: true,
    });
  } catch (err) {
    console.error("Error in updateRoadmap:", err);
    res.status(500).json({
      message: "Something went wrong while updating Roadmap",
      success: false,
      error: err,
    });
  }
};

const getAllRoadmaps = async (req: Request, res: Response): Promise<void> => {
  try {
    const lucid = await initializeLucid();
    const cbor = process.env.CBOR!;
    const RefiScript: Script = {
      type: "PlutusV2",
      script: cbor,
    };

    const RefiAddress = lucid.utils.validatorToAddress(RefiScript);
    const utxos = await lucid.utxosAt(RefiAddress);
    // console.log("utxos", utxos);

    const allDetails: Array<ProjectDatum> = [];

    utxos
      .map((utxo) => {
        // Decode datum with proper typing
        if (!utxo.datum) return;
        const decodedDatum = Data.from(utxo.datum) as Constr<Data>;
        // console.log("Decoded Datum:", decodedDatum);
        let progress = Number(decodedDatum.fields[4] as bigint);
        if (progress > 0) {
          progress /= 100;
        }
        const data = {
          preId: toText(decodedDatum.fields[0] as string),
          roadmapId: toText(decodedDatum.fields[1] as string),
          roadmapName: toText(decodedDatum.fields[2] as string),
          roadmapDescription: toText(decodedDatum.fields[3] as string),
          progress: progress,
          adminPkh: decodedDatum.fields[5] as string, // keep hex
          prePkh: decodedDatum.fields[6] as string, // keep hex
          preSkh: decodedDatum.fields[7] as string, // keep hex
          totalPlasticCredits: Number(decodedDatum.fields[8] as bigint),
          soldPlasticCredits: Number(decodedDatum.fields[9] as bigint),
          totalPlasticTokens: Number(decodedDatum.fields[10] as bigint),
          sentPlasticTokens: Number(decodedDatum.fields[11] as bigint),
          totalPlastic: Number(decodedDatum.fields[12] as bigint),
          recoveredPlastic: Number(decodedDatum.fields[13] as bigint),
        };
        allDetails.push(data);
      })
      .filter(Boolean);

    res.status(200).json({
      message: "Fetched all roadmaps successfully",
      roadmaps: allDetails,
      success: true,
    });
  } catch (err) {
    console.error(err);
    res.status(500).json({
      message: "Something went wrong while fetching roadmaps",
      error: err,
      success: false,
    });
  }
};

const releaseFunds = async (req: Request, res: Response): Promise<void> => {
  try {
    const { preId, roadmapId }: ReleaseFundsRequest = req.body;
    const lucid = await initializeLucid();
    const adminSeed = process.env.ADMIN_SEED!;
    lucid.selectWalletFromSeed(adminSeed);
    const adminAddress = await lucid.wallet.address();
    // 1. Initialize wallet and scripts
    const RefiScript: Script = {
      type: "PlutusV2",
      script: process.env.CBOR!,
    };
    const RefiAddress = lucid.utils.validatorToAddress(RefiScript);

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
      res.status(404).json({ error: "Roadmap UTXO not found", success: false });
      return;
    }

    const redeemer = Data.to(new Constr(1, []));
    const plastikAssetId: string = process.env.PLASTIK || "";
    const usdmAssetId: string = process.env.USDM || "";
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
    // console.log(preAddress);
    const plastikValue = BigInt(matchedUtxo.assets[plastikAssetId]);
    // console.log(plastikValue);
    // 4. Sent 2% of plastik token(which will change through governance) locked in contract to deadWallet
    const deadWallet = process.env.DEAD_WALLET_ADDRESS!;
    const deadWalletValue = (plastikValue * 2n) / 100n;
    // console.log("deadWalletValue", deadWalletValue);

    const plastikTokenSendBackToAdmin = plastikValue - deadWalletValue;
    // console.log("plastikTokenSendBackToAdmin", plastikTokenSendBackToAdmin);

    const usdmValue = plastikTokenSendBackToAdmin / 100n;
    // console.log("usdmValue", usdmValue);

    const tx = await lucid
      .newTx()
      .collectFrom([matchedUtxo], redeemer)
      .attachSpendingValidator(RefiScript)
      .addSigner(adminAddress)
      .payToAddress(adminAddress, {
        lovelace: matchedUtxo.assets.lovelace,
        [plastikAssetId]: plastikTokenSendBackToAdmin,
      })
      .payToAddress(deadWallet, {
        [plastikAssetId]: deadWalletValue,
      })
      .payToAddress(preAddress, {
        [usdmAssetId]: usdmValue,
      })
      .complete();

    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();

    // 5. Save the Roadmap to CompletedRoadmap table
    const completedRoadmap = {
      preId: toText(oldDatum.fields[0] as string),
      roadmapId: toText(oldDatum.fields[1] as string),
      roadmapName: toText(oldDatum.fields[2] as string),
      roadmapDescription: toText(oldDatum.fields[3] as string),
      progress: Number(oldDatum.fields[4] as bigint) / 100,
      preAddress: preAddress,
      totalPlasticCredits: Number(oldDatum.fields[8] as bigint),
      soldPlasticCredits: Number(oldDatum.fields[9] as bigint),
      totalPlasticTokens: Number(oldDatum.fields[10] as bigint),
      sentPlasticTokens: Number(oldDatum.fields[11] as bigint),
      totalPlastic: Number(oldDatum.fields[12] as bigint),
      recoveredPlastic: Number(oldDatum.fields[13] as bigint),
    };
    await CompletedRoadmap.create(completedRoadmap);
    // console.log("Completed Roadmap:", completedRoadmap);

    res.status(201).json({
      message: "Funds released successfully",
      success: true,
      txHash: txHash,
    });
    return;
  } catch (error) {
    res.status(500).json({
      message: "Something went wrong during releasing funds",
      success: false,
      error: error instanceof Error ? error.message : "Unknown error",
    });
  }
};

const queryTransaction = async (req: Request, res: Response): Promise<void> => {
  try {
    const { txHash, outputIndex }: QueryTransaction = req.body;
    const lucid = await initializeLucid();
    const outRef: OutRef[] = [
      {
        txHash,
        outputIndex,
      },
    ];
    const utxo = await lucid.provider.getUtxosByOutRef(outRef);
    // console.log("utxo", utxo);
    if (!utxo || utxo.length == 0) {
      res.status(404).json({
        message: "Utxo not found wait for sometime or check your txHash",
        success: false,
      });
      return;
    }
    const decodedDatum = Data.from(utxo[0].datum!) as Constr<Data>;
    const data = {
      txHash,
      assets: JSON.parse(JSON.stringify(utxo[0].assets, bigIntReplacer)),
      preId: toText(decodedDatum.fields[0] as string),
      roadmapId: toText(decodedDatum.fields[1] as string),
      roadmapName: toText(decodedDatum.fields[2] as string),
      roadmapDescription: toText(decodedDatum.fields[3] as string),
      progress: Number(decodedDatum.fields[4] as bigint),
      totalPlasticCredits: Number(decodedDatum.fields[8] as bigint),
      soldPlasticCredits: Number(decodedDatum.fields[9] as bigint),
      totalPlasticTokens: Number(decodedDatum.fields[10] as bigint),
      sentPlasticTokens: Number(decodedDatum.fields[11] as bigint),
      totalPlastic: Number(decodedDatum.fields[12] as bigint),
      recoveredPlastic: Number(decodedDatum.fields[13] as bigint),
    };

    res.status(200).json({
      message: "Utxo fetched succesfully",
      success: true,
      data,
    });
  } catch (error) {
    res.status(500).json({
      message: "Something went wrong while querying transaction",
      success: false,
      error: error instanceof Error ? error.message : "Unknown error",
    });
  }
};

const queryAddressHistory = async (
  req: Request,
  res: Response
): Promise<void> => {
  try {
    const { address } = req.body;
    const { page = 1, count = 10, order = "desc" } = req.query;

    if (!address) {
      res.status(400).json({ message: "Address is required" });
      return;
    }

    const blockfrostUrl = process.env.BLOCKFROST_URL;
    const BLOCKFROST_API_KEY = process.env.BLOCKFROST_PROJECT_ID;
    const pcAssetId = process.env.PC_ASSET_ID!;

    // Step 1: Fetch transaction hashes
    const txHashesRes = await axios.get(
      `${blockfrostUrl}/addresses/${address}/transactions`,
      {
        headers: { project_id: BLOCKFROST_API_KEY },
        params: { page, count, order },
      }
    );

    const allTransactions = txHashesRes.data;

    // Step 2: Fetch only UTXO info (to reduce API usage)
    const transactions = await Promise.all(
      allTransactions.map(async (transaction: any) => {
        const txUtxoRes = await axios.get(
          `${blockfrostUrl}/txs/${transaction.tx_hash}/utxos`,
          {
            headers: { project_id: BLOCKFROST_API_KEY },
          }
        );

        const tx = txUtxoRes.data;

        let inputAda = 0,
          outputAda = 0;
        let inputPC = 0,
          outputPC = 0;

        // Process inputs
        tx.inputs.forEach((input: any) => {
          if (input.address === address) {
            input.amount.forEach((amt: any) => {
              if (amt.unit === "lovelace") inputAda += Number(amt.quantity);
              if (amt.unit === pcAssetId) inputPC += Number(amt.quantity);
            });
          }
        });

        // Process outputs
        tx.outputs.forEach((output: any) => {
          if (output.address === address) {
            output.amount.forEach((amt: any) => {
              if (amt.unit === "lovelace") outputAda += Number(amt.quantity);
              if (amt.unit === pcAssetId) outputPC += Number(amt.quantity);
            });
          }
        });

        // Net values and direction
        const netAda = outputAda - inputAda;
        const netPC = outputPC - inputPC;
        const direction =
          netAda > 0 ? "received" : netAda < 0 ? "sent" : "self";

        // Estimate fee = total input - total output (ADA only)
        const totalInputAda = tx.inputs.reduce((sum: number, input: any) => {
          const lovelace = input.amount.find(
            (amt: any) => amt.unit === "lovelace"
          );
          return sum + (lovelace ? Number(lovelace.quantity) : 0);
        }, 0);
        const totalOutputAda = tx.outputs.reduce((sum: number, output: any) => {
          const lovelace = output.amount.find(
            (amt: any) => amt.unit === "lovelace"
          );
          return sum + (lovelace ? Number(lovelace.quantity) : 0);
        }, 0);
        const fee = (totalInputAda - totalOutputAda) / 1_000_000;

        return {
          date: new Date(transaction.block_time * 1000).toLocaleDateString(
            "en-US"
          ),
          transactionFee: `${fee.toFixed(2)} ADA`,
          amount: `${(Math.abs(netAda) / 1_000_000).toFixed(2)} ADA ${
            Math.abs(netPC) > 0 ? "+ " + Math.abs(netPC) + " P.C" : ""
          }`,
          tokenId: netPC > 0 ? pcAssetId : "N/A",
          direction,
          pcAssetId:
            Math.abs(netPC) > 0
              ? `https://preprod.cexplorer.io/asset/${pcAssetId}`
              : "N/A",
          hash: `https://preprod.cardanoscan.io/transaction/${transaction.tx_hash}`,
        };
      })
    );
    console.log("Api Called");

    res.status(200).json({
      message: "Transaction history fetched successfully",
      data: transactions,
    });
  } catch (error: any) {
    console.error(
      "Error querying transaction details:",
      error?.response?.data || error.message
    );
    res.status(500).json({
      message: "Failed to fetch transaction history",
      error: error?.response?.data || error.message,
    });
  }
};

const getAllCompletedRoadmaps = async (
  req: Request,
  res: Response
): Promise<void> => {
  try {
    // Query all completed roadmaps from the database
    const completedRoadmaps = await CompletedRoadmap.findAll();

    // If no roadmaps found, send an empty array with a 200 status
    if (!completedRoadmaps || completedRoadmaps.length === 0) {
      res.status(200).json({
        message: "No completed roadmaps found",
        roadmaps: [],
      });
    } else {
      // Send the retrieved roadmaps in the response
      res.status(200).json({
        message: "Completed roadmaps retrieved successfully",
        roadmaps: completedRoadmaps,
      });
    }
  } catch (error) {
    // Handle errors, such as database connection issues, etc.
    console.error("Error retrieving completed roadmaps:", error);
    res.status(500).json({
      message: "An error occurred while retrieving completed roadmaps",
      error: error instanceof Error ? error.message : "Unknown error",
    });
  }
};

export {
  initializeRoadmap,
  updateRoadmap,
  getAllRoadmaps,
  releaseFunds,
  queryTransaction,
  queryAddressHistory,
  getAllCompletedRoadmaps,
  initializeLucid,
};
