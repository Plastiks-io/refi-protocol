import { Blockfrost, C, Lucid } from "lucid-cardano";
import { Request, Response } from "express";
import { Transaction, TransactionType } from "../models/transaction.model.js";
import { config } from "dotenv";
config();

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

const sentPC = async (req: Request, res: Response) => {
  try {
    const { address, amount } = req.body;
    if (!address || !amount) {
      res.status(400).json({
        message: "Address and amount are required",
        success: false,
      });
      return;
    }
    const lucid = await initializeLucid();
    const seed = process.env.PC_WALLET!;
    lucid.selectWalletFromSeed(seed);
    // console.log(amount);

    const pcAssetId = process.env.PC_ASSET_ID!;
    const usdmAssetId = process.env.USDM!;

    // send minLovelace along with PC token
    const tx = await lucid
      .newTx()
      .payToAddress(address, {
        [pcAssetId]: BigInt(amount),
      })
      .complete();

    const txFee = tx.fee;
    console.log(txFee);
    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();

    // Create a new transaction in the database
    await Transaction.create({
      txDate: new Date(),
      txFee: txFee,
      amount,
      assetId: pcAssetId,
      hash: txHash,
      type: TransactionType.Sold,
    });
    res.status(200).json({
      message: "Transaction successful",
      txHash,
      success: true,
    });
    return;
  } catch (error) {
    console.error("Error sending transaction:", error);
    res.status(500).json({
      message: "Transaction failed",
      error: error instanceof Error ? error.message : "Unknown error",
    });
  }
};

export { sentPC };
