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

    // 409 Conflict
    if (matchedUtxo) {
      res.status(409).json({
        message: "Roadmap already exists",
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

    console.log("Datum to lock:", datumToLock);
    console.log("Refi Address:", RefiAddress);
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
      error: "Error occured during the creation of roadmap",
      success: false,
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

    console.log("matchedUtxo", matchedUtxo);

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
    console.error("❌ Error in updateRoadmap:", err);
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
    const allDetails: Array<ProjectDatum> = [];

    utxos
      .map((utxo) => {
        if (!utxo.datum) return null;

        // Decode datum with proper typing
        const decodedDatum = Data.from(utxo.datum) as Constr<Data>;
        console.log("Decoded Datum:", decodedDatum);
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
    res.status(500).json({ error: "Internal server error" });
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

    console.log("matchedUtxo", matchedUtxo);

    if (!matchedUtxo) {
      res.status(404).json({ error: "Roadmap UTXO not found" });
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
    console.log(preAddress);
    const plastikValue = BigInt(matchedUtxo.assets[plastikAssetId]);
    console.log(plastikValue);
    // 4. Sent 2% of plastik token(which will change through governance) locked in contract to deadWallet
    const deadWallet = process.env.DEAD_WALLET_ADDRESS!;
    const deadWalletValue = (plastikValue * 2n) / 100n;
    console.log("deadWalletValue", deadWalletValue);

    const plastikTokenSendBackToAdmin = plastikValue - deadWalletValue;
    console.log("plastikTokenSendBackToAdmin", plastikTokenSendBackToAdmin);

    const usdmValue = plastikTokenSendBackToAdmin / 100n;
    console.log("usdmValue", usdmValue);

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
    const { txHash }: QueryTransaction = req.body;
    const lucid = await initializeLucid();
    const outRef: OutRef[] = [
      {
        txHash,
        outputIndex: 0,
      },
    ];
    const utxo = await lucid.provider.getUtxosByOutRef(outRef);
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

    console.log(utxo);
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

const queryAddress = async (req: Request, res: Response): Promise<void> => {
  try {
    const { address } = req.body;
    const lucid = await initializeLucid();
    const utxos = await lucid.provider.getUtxos(address);
    res.status(200).json({
      message: "",
    });
  } catch (error) {}
};
export {
  initializeRoadmap,
  updateRoadmap,
  getAllRoadmaps,
  releaseFunds,
  queryTransaction,
};
