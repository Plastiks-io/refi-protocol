import axios from "axios";
import { BrowserWallet, Transaction } from "@meshsdk/core";
import { toast } from "sonner";

const sendNft = async (wallet: BrowserWallet, quantity: number) => {
  try {
    const adminAddress = import.meta.env.VITE_ADMIN_WALLET_ADDRESS;

    if (!adminAddress || !adminAddress.startsWith("addr")) {
      throw new Error("Invalid or missing admin wallet address.");
    }
    console.log("quantity", quantity);

    const qty = String(quantity * 1_000_000); // convert ADA to lovelace
    console.log("qty", qty);

    const tx = new Transaction({
      initiator: wallet,
    }).sendAssets(
      {
        address: adminAddress,
      },
      [
        {
          unit: "lovelace",
          quantity: qty,
        },
      ]
    );

    const unsignedTx = await tx.build();
    const signedTx = await wallet.signTx(unsignedTx);
    const txHash = await wallet.submitTx(signedTx);

    console.log("Transaction Hash:", txHash);

    const userAddress = await wallet.getChangeAddress();
    console.log("User Address:", userAddress);

    // send NFT to the user
    const { data: resp } = await axios.post(
      `${import.meta.env.VITE_SERVER_URL}/user/send-pc`,
      { address: userAddress, amount: quantity }
    );
    console.log(resp.message);
    return txHash;
  } catch (error) {
    console.error("Error sending NFT:", error);
    toast.error("Failed to send NFT");
    throw new Error("Failed to send NFT");
  }
};

const updateRoadmapData = async (
  preId: string,
  roadmapId: string,
  quantity: number
) => {
  const url = import.meta.env.VITE_SERVER_URL;
  const apiUrl = `${url}/roadmap/update`;
  console.log(apiUrl);
  const data = {
    preId,
    roadmapId,
    soldPlasticCredit: quantity,
  };

  const { data: resp } = await axios.post(apiUrl, data);
  console.log(resp);
  if (resp.success) {
    console.log("Roadmap data updated successfully");
  }
  return resp;
};

const waitForTransactionConfirmation = async (
  txHash: string,
  maxRetries = 20,
  interval = 5000
): Promise<boolean> => {
  for (let attempt = 0; attempt < maxRetries; attempt++) {
    try {
      const res = await axios.get(
        `https://cardano-preprod.blockfrost.io/api/v0/txs/${txHash}`,
        {
          headers: {
            project_id: "preprodN1EZYj11zL89jJeaAjeRybxYMLp7grmn",
          },
        }
      );
      const status = res.data.valid_contract;
      console.log("Transaction status:", status);
      if (status) {
        console.log("Transaction confirmed");
        toast.success("Roadmap data updated successfully");
        return true;
      } else {
        console.log("Transaction not confirmed yet");
      }
    } catch (e) {}

    await new Promise((resolve) => setTimeout(resolve, interval));
  }

  return false;
};

export { updateRoadmapData, waitForTransactionConfirmation, sendNft };
