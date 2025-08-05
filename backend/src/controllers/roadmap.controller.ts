import { Request, Response } from "express";
import {
  Lucid,
  Blockfrost,
  Script,
  Constr,
  fromText,
  Data,
  toText,
} from "lucid-cardano";
import dotenv from "dotenv";
import {
  InitializeRoadmapRequest,
  ProjectDatum,
} from "../types/roadmap.types.js";
dotenv.config();
import CompletedRoadmap from "../models/completedRoadmap.model.js";
import ArchivedRoadmap from "../models/archivedRoadmap.model.js";
import { getPubKeyHash } from "./stakeReward.controller.js";
import { refiValidator } from "../contract/contracts.js";

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
    }: InitializeRoadmapRequest = req.body;

    if (
      !preId ||
      !roadmapId ||
      !roadmapName ||
      !roadmapDescription ||
      !prePkh ||
      !preSkh ||
      !totalPlasticCredits
    ) {
      res.status(400).json({ error: "Missing required fields" });
      return;
    }

    const lucid = await initializeLucid();

    const adminSeed: string = process.env.ADMIN_SEED!;
    lucid.selectWalletFromSeed(adminSeed);
    const adminsPkh = await getPubKeyHash(lucid);

    const RefiAddress = lucid.utils.validatorToAddress(refiValidator);

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
    // check if a roadmap already exist with same preId and roadmapId in archived roadmap
    const archived_Roadmap = await ArchivedRoadmap.findOne({
      where: {
        preId: preId,
        roadmapId: roadmapId,
      },
    });
    if (completedRoadmap || archived_Roadmap) {
      res.status(409).json({
        message: "Roadmap already exists",
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
    // Total plastik token will be 100 * totalPlasticCredits
    const totalPlasticTokens = totalPlasticCredits * 100;
    // Total Plastic to recovered = Total Plastik credit
    const totalPlastic = totalPlasticCredits;

    // Datum to be locked for the particular roadmap
    const datumToLock = new Constr(0, [
      fromText(preId || ""),
      fromText(roadmapId || ""),
      fromText(roadmapName || ""),
      fromText(roadmapDescription || ""),
      BigInt(0),
      [adminsPkh],
      prePkh,
      preSkh,
      BigInt(totalPlasticCredits),
      BigInt(0),
      BigInt(totalPlasticTokens),
      BigInt(0),
      BigInt(totalPlastic),
      BigInt(0),
      fromText(new Date().toISOString()),
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

const getAllActiveRoadmaps = async (
  req: Request,
  res: Response
): Promise<void> => {
  try {
    const lucid = await initializeLucid();
    const cbor = process.env.REFI_CBOR!;
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
        // extract address from prePkh and preSkh
        const paymentCredential = lucid.utils.keyHashToCredential(
          decodedDatum.fields[6] as string
        );
        const stakeCredential = lucid.utils.keyHashToCredential(
          decodedDatum.fields[7] as string
        );
        const address = lucid.utils.credentialToAddress(
          paymentCredential,
          stakeCredential
        );
        const precisionFactor = 1_000_000n; // 1 PC = 1,000,000 micro PC
        const ptPolicyId = process.env.POLICY_ID!;
        const ptAssetName = process.env.PLASTIC_TOKEN_NAME!;
        const ptAssetUnit = `${ptPolicyId}${ptAssetName}`;
        const fundsMissing =
          ((utxo.assets[ptAssetUnit] ?? 0n) * precisionFactor) / 100n;

        const usdmPolicyId = process.env.POLICY_ID!;
        const usdmTokenName = process.env.USDM_TOKEN_NAME!;
        const usdmAssetUnit: string = `${usdmPolicyId}${usdmTokenName}`;
        const fundsDistributed =
          utxo.assets[usdmAssetUnit] ?? 0n / precisionFactor;

        const data = {
          preId: toText(decodedDatum.fields[0] as string),
          roadmapId: toText(decodedDatum.fields[1] as string),
          roadmapName: toText(decodedDatum.fields[2] as string),
          roadmapDescription: toText(decodedDatum.fields[3] as string),
          progress: progress,
          preAddress: address,
          totalPlasticCredits: Number(decodedDatum.fields[8] as bigint),
          soldPlasticCredits: Number(decodedDatum.fields[9] as bigint),
          totalPlasticTokens: Number(decodedDatum.fields[10] as bigint),
          sentPlasticTokens: Number(decodedDatum.fields[11] as bigint),
          totalPlastic: Number(decodedDatum.fields[12] as bigint),
          recoveredPlastic: Number(decodedDatum.fields[13] as bigint),
          createdAt: toText(decodedDatum.fields[14] as string),
          status: "active",
          fundsMissing: fundsMissing.toString(),
          fundsDistributed: fundsDistributed.toString(),
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

const saveRoadmap = async (req: Request, res: Response): Promise<void> => {
  try {
    // Expecting full roadmap object under `roadmap` key in the request body
    const roadmap = req.body;

    // console.log(roadmap);

    // Destructure and type-convert as needed
    const {
      preId,
      roadmapId,
      roadmapName,
      roadmapDescription,
      progress,
      preAddress,
      totalPlasticCredits,
      soldPlasticCredits,
      totalPlasticTokens,
      sentPlasticTokens,
      totalPlastic,
      recoveredPlastic,
      createdAt,
    } = roadmap;

    // Create and persist the document
    const completedRoadmap = await CompletedRoadmap.create({
      preId,
      roadmapId,
      roadmapName,
      roadmapDescription,
      progress,
      preAddress,
      totalPlasticCredits,
      soldPlasticCredits,
      totalPlasticTokens,
      sentPlasticTokens,
      totalPlastic,
      recoveredPlastic,
      createdAt: createdAt ? new Date(createdAt) : new Date(),
    });

    res.status(201).json({
      message: "Roadmap saved to DB successfully",
      success: true,
      data: completedRoadmap,
    });
  } catch (error) {
    console.error("Error saving roadmap:", error);
    res.status(500).json({
      message: "Something went wrong during saving roadmap to DB",
      success: false,
      error: error instanceof Error ? error.message : "Unknown error",
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

const saveArchivedRoadmap = async (req: Request, res: Response) => {
  try {
    // Expecting full roadmap object under `roadmap` key in the request body
    const roadmap = req.body;

    // console.log("roadmap", roadmap);

    // Destructure and type-convert as needed
    const {
      preId,
      roadmapId,
      roadmapName,
      roadmapDescription,
      progress,
      preAddress,
      totalPlasticCredits,
      soldPlasticCredits,
      totalPlasticTokens,
      sentPlasticTokens,
      totalPlastic,
      recoveredPlastic,
      createdAt,
    } = roadmap;
    // 5. Save the Roadmap to CompletedRoadmap table
    const archived_roadmap = {
      preId,
      roadmapId,
      roadmapName,
      roadmapDescription,
      progress,
      preAddress,
      totalPlasticCredits,
      soldPlasticCredits,
      totalPlasticTokens,
      sentPlasticTokens,
      totalPlastic,
      recoveredPlastic,
      createdAt: createdAt ? new Date(createdAt) : new Date(),
    };
    await ArchivedRoadmap.create(archived_roadmap);

    res.status(201).json({
      message: "Roadmap Saved in Archived DB successfully",
      success: true,
    });
  } catch (error) {
    console.error("Error saving archived roadmap:", error);
    res.status(500).json({
      message: "Something went wrong during saving archived roadmap to DB",
      success: false,
      error: error instanceof Error ? error.message : "Unknown error",
    });
  }
};

const restoreRoadmap = async (req: Request, res: Response) => {
  try {
    const id = req.params.id;
    // 2. Find the Archived Roadmap by id
    const archived_Roadmap = await ArchivedRoadmap.findOne({
      where: { id },
    });

    if (!archived_Roadmap) {
      res.status(404).json({ error: "Archived roadmap not found" });
      return;
    }

    await ArchivedRoadmap.destroy({ where: { id } });

    res.status(201).json({
      message: "Roadmap restored successfully",
      success: true,
    });
  } catch (error) {
    console.error("Error restoring roadmap:", error);
    res.status(500).json({
      message: "An error occurred while restoring roadmap",
      error: error instanceof Error ? error.message : "Unknown error",
    });
  }
};

const getAllArchivedRoadmaps = async (req: Request, res: Response) => {
  try {
    const archived_roadmaps = await ArchivedRoadmap.findAll();
    res.status(200).json({
      archived_roadmaps,
      message: "All archived roadmaps fetched successfully",
    });
  } catch (error) {
    console.error("Error getting all archived roadmaps:", error);
    res.status(500).json({
      message: "An error occurred while getting all archived roadmaps",
      error: error instanceof Error ? error.message : "Unknown error",
    });
  }
};

const deleteArchivedRoadmap = async (req: Request, res: Response) => {
  try {
    const id = req.params.id;
    const archived_roadmap = await ArchivedRoadmap.findOne({ where: { id } });

    if (!archived_roadmap) {
      res.status(404).json({ error: "Archived roadmap not found" });
      return;
    }

    await archived_roadmap.destroy();
    res.status(200).json({
      message: "Archived roadmap deleted successfully",
      success: true,
    });
  } catch (error) {
    console.error("Error deleting archived roadmap:", error);
    res.status(500).json({
      message: "An error occurred while deleting archived roadmap",
      error: error instanceof Error ? error.message : "Unknown error",
    });
  }
};

export {
  initializeRoadmap,
  getAllActiveRoadmaps,
  saveRoadmap,
  getAllCompletedRoadmaps,
  initializeLucid,
  saveArchivedRoadmap,
  getAllArchivedRoadmaps,
  restoreRoadmap,
  deleteArchivedRoadmap,
};
