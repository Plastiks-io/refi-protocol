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
  Assets,
} from "lucid-cardano";
import dotenv from "dotenv";
import {
  ArchivedRoadmapRequest,
  InitializeRoadmapRequest,
  ProjectDatum,
  QueryTransaction,
  ReleaseFundsRequest,
  UpdateRoadmapRequest,
} from "../types/roadmap.types.js";
dotenv.config();
import CompletedRoadmap from "../models/completedRoadmap.model.js";
import ArchivedRoadmap from "../models/archivedRoadmap.model.js";
import {
  buildLenderAction,
  buildLenderDatum,
  getPubKeyHash,
  parseLenderDatum,
  parseRoadmapDatum,
} from "./stakeReward.controller.js";
import { refiValidator, stakeRewardValidator } from "../contract/contracts.js";
import { LenderDatum } from "../types/stake.reward.types.js";

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

// const updateRoadmap = async (req: Request, res: Response): Promise<void> => {
//   try {
//     const { preId, roadmapId, soldPlasticCredit }: UpdateRoadmapRequest =
//       req.body;
//     const lucid = await initializeLucid();
//     const adminSeed = process.env.ADMIN_SEED!;
//     lucid.selectWalletFromSeed(adminSeed);
//     const adminAddress = await lucid.wallet.address();

//     // Find matching UTXO at refi contract
//     const refiContractAddress = lucid.utils.validatorToAddress(refiValidator);
//     const utxos = await lucid.utxosAt(refiContractAddress);
//     const matchedUtxo = utxos.find((u) => {
//       if (!u.datum) return false;
//       const d = Data.from(u.datum) as Constr<Data>;
//       return (
//         toText(d.fields[0] as string) === preId &&
//         toText(d.fields[1] as string) === roadmapId
//       );
//     });
//     if (!matchedUtxo?.datum) throw new Error("No matching roadmap UTxO");

//     const old = parseRoadmapDatum(Data.from(matchedUtxo.datum));
//     const newSold = old.soldPlasticCredits + BigInt(soldPlasticCredit);
//     const progress = (newSold * 10000n) / old.totalPlasticCredits;
//     const newSent = (progress * old.totalPlasticTokens) / 10000n;
//     const recovered = (progress * old.totalPlastic) / 10000n;
//     const updatedDatum = new Constr(0, [
//       fromText(old.preId),
//       fromText(old.roadmapId),
//       fromText(old.roadmapName),
//       fromText(old.roadmapDescription),
//       BigInt(progress),
//       old.adminsPkh,
//       old.prePkh,
//       old.preSkh,
//       BigInt(old.totalPlasticCredits),
//       BigInt(newSold),
//       BigInt(old.totalPlasticTokens),
//       BigInt(newSent),
//       BigInt(old.totalPlastic),
//       BigInt(recovered),
//       fromText(old.createdAt),
//     ]);

//     // Prepare reward contract UTxO
//     const stakeAddr = lucid.utils.validatorToAddress(stakeRewardValidator);
//     const stakeUtxos = await lucid.utxosAt(stakeAddr);
//     if (stakeUtxos.length === 0) throw new Error("No stake UTxOs found");
//     const stakeUtxo = stakeUtxos[0];
//     if (!stakeUtxo.datum) throw new Error("Missing stake datum");
//     const lender = parseLenderDatum(Data.from(stakeUtxo.datum));

//     const adminPkh = await getPubKeyHash(lucid);
//     if (!lender.adminsPkh.includes(adminPkh)) throw new Error("Unauthorized");

//     const ptUnit = process.env.PLASTIC_TOKEN!;
//     const needPT = BigInt(soldPlasticCredit) * 80n;
//     // sum PT in this single UTxO
//     const contractPT = stakeUtxo.assets[ptUnit] || 0n;

//     // Collect any extra from admin if contractPT insufficient
//     let extraAdminUtxo;
//     if (contractPT < needPT) {
//       const adminUtxos = await lucid.utxosAt(adminAddress);
//       extraAdminUtxo = adminUtxos.find(
//         (u) => (u.assets[ptUnit] || 0n) >= needPT - contractPT
//       );
//       if (!extraAdminUtxo) throw new Error("Admin has insufficient PT");
//     }

//     // Build redeemers and new lender datum
//     const refiRedeemer = Data.to(new Constr(0, [progress]));
//     const lenderRedeemer = Data.to(
//       buildLenderAction({ type: "FundPlastikToEscrow", amount: needPT })
//     );
//     // 1. Compute the *new* reward from this sale only:
//     const precision = 1_000_000n;
//     const rewardMicro = (BigInt(soldPlasticCredit) * precision * 2n) / 100n;
//     // ▸ ΔR in “micro‑USDM” units

//     // 2. Distribute ΔR proportionally to existing stakes:
//     const updatedLenders = lender.lenders.map(
//       ([pk, [bal, oldDebt]]): [string, [bigint, bigint]] => {
//         const share =
//           lender.totalPT > 0n
//             ? (bal * rewardMicro) / lender.totalPT // bal/T_old * ΔR
//             : 0n;
//         return [pk, [bal, oldDebt + share]];
//       }
//     );

//     // 3. Update your LenderDatum
//     const newLenderDatum: LenderDatum = {
//       adminsPkh: lender.adminsPkh,
//       totalPT: lender.totalPT, // stakes haven’t changed
//       totalReward: lender.totalReward + rewardMicro,
//       lenders: updatedLenders,
//     };

//     const usdmAssetUnit: string = process.env.USDM_TOKEN!;
//     // Build assets for refi and stake outputs
//     const refiAssets: Assets = {
//       lovelace: matchedUtxo.assets.lovelace,
//       [ptUnit]: (matchedUtxo.assets[ptUnit] || 0n) + needPT,
//     };
//     if (matchedUtxo.assets[usdmAssetUnit]) {
//       refiAssets[usdmAssetUnit] = matchedUtxo.assets[usdmAssetUnit];
//     }

//     const stakeAssets: Assets = {
//       [usdmAssetUnit]: (stakeUtxo.assets[usdmAssetUnit] || 0n) + rewardMicro,
//     };
//     if (stakeUtxo.assets[ptUnit] - needPT >= 0n) {
//       stakeAssets[ptUnit] = (stakeUtxo.assets[ptUnit] || 0n) - needPT;
//     }
//     // console.log(stakeAssets);

//     // Build Tx
//     let txBuilder = lucid
//       .newTx()
//       // refi update
//       .collectFrom([matchedUtxo], refiRedeemer)
//       .attachSpendingValidator(refiValidator)
//       .payToContract(
//         lucid.utils.validatorToAddress(refiValidator),
//         { inline: Data.to(updatedDatum) },
//         refiAssets
//       )
//       // stake payout
//       .collectFrom([stakeUtxo], lenderRedeemer);

//     // if extra admin funds needed, collect from admin UTxO
//     if (extraAdminUtxo) {
//       txBuilder = txBuilder.collectFrom([extraAdminUtxo]);
//     }

//     txBuilder = txBuilder
//       .attachSpendingValidator(stakeRewardValidator)
//       .payToContract(
//         stakeAddr,
//         { inline: Data.to(buildLenderDatum(newLenderDatum)) },
//         stakeAssets
//       )
//       .addSigner(adminAddress);

//     const tx = await txBuilder.complete();
//     const signed = await tx.sign().complete();
//     const hash = await signed.submit();
//     ``;

//     res.status(200).json({
//       message: "Roadmap updated",
//       txHash: hash,
//       newProgress: Number(progress) / 100,
//       success: true,
//     });
//   } catch (err) {
//     console.error(err);
//     res.status(500).json({
//       message: "Failed to update roadmap",
//       success: false,
//       error: err instanceof Error ? err.message : err,
//     });
//   }
// };

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
        const ptAssetUnit = process.env.PLASTIC_TOKEN!;
        const fundsMissing =
          ((utxo.assets[ptAssetUnit] ?? 0n) * precisionFactor) / 100n;

        const usdmAssetUnit: string = process.env.USDM_TOKEN!;
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
  // updateRoadmap,
  saveRoadmap,
  queryTransaction,
  queryAddressHistory,
  getAllCompletedRoadmaps,
  initializeLucid,
  saveArchivedRoadmap,
  getAllArchivedRoadmaps,
  restoreRoadmap,
  deleteArchivedRoadmap,
};
