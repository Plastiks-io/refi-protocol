import { Blockfrost, C, Lucid } from "lucid-cardano";
import { Request, Response } from "express";
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
    const lucid = await initializeLucid();
    const seed = process.env.PC_WALLET!;
    lucid.selectWalletFromSeed(seed);
    const { address, amount } = req.body;
    console.log(amount);

    const pcAssetId = process.env.PC_ASSET_ID!;

    // send minLovelace along with PC token
    const tx = await lucid
      .newTx()
      .payToAddress(address, {
        [pcAssetId]: BigInt(amount),
      })
      .complete();

    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();
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
