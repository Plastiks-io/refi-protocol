import React, { useContext, useEffect, useState } from "react";
import { Loader2 } from "lucide-react";
import { WalletContext } from "../../App";
import {
  sendNft,
  updateRoadmapData,
  waitForTransactionConfirmation,
} from "../../services/BuyNFT";
import { toast } from "sonner";
import { useDispatch, useSelector } from "react-redux";
import { RootState, AppDispatch } from "../../redux/store";
import { fetchRoadmaps } from "../../redux/roadmapSlice";

const NFTPurchase: React.FC = () => {
  const wallet = useContext(WalletContext);

  const [loadingIndex, setLoadingIndex] = useState<number | null>(null);
  const [quantities, setQuantities] = useState<{ [index: number]: number }>({});
  const [txToConfirm, setTxToConfirm] = useState<string | null>(null);

  const [address, setAddress] = useState<string | null>(null);
  const { roadmaps, loading, error } = useSelector(
    (state: RootState) => state.roadmaps
  );

  const dispatch = useDispatch<AppDispatch>();

  const onBuy = async (index: number) => {
    try {
      const qty = quantities[index] || 1;
      const availableCredits =
        roadmaps[index].totalPlasticCredits -
        roadmaps[index].soldPlasticCredits;

      if (qty > availableCredits) {
        toast.error(
          `You can only buy up to ${availableCredits} plastic credits for this roadmap.`
        );
        return;
      }
      if (!wallet) {
        throw new Error("Wallet is not connected.");
      }
      // const txHash = await sendNft(wallet, qty);
      const txHash = await sendNft(wallet, qty);
      console.log("Transaction Hash:", txHash);
      setQuantities((prev) => ({ ...prev, [index]: 1 }));
      toast.success("NFT purchased successfully!");
      const resp = await updateRoadmapData(
        roadmaps[index].preId,
        roadmaps[index].roadmapId,
        qty
      );
      console.log(resp.txHash);
      // Set the transaction hash to confirm outside
      setTxToConfirm(resp.txHash);
    } catch (error) {
      console.error("Error purchasing NFT:", error);
      const errorMessage =
        error instanceof Error ? error.message : "An unknown error occurred";
      toast.error(`Failed to purchase NFT: ${errorMessage}`);
    }
  };

  const handleBuy = async (index: number) => {
    if (!wallet || !address) {
      toast.warning("Please connect your wallet before buying.");
      return;
    }

    setLoadingIndex(index);
    try {
      await onBuy(index);
    } catch (err) {
      console.error("Buy failed", err);
    } finally {
      setLoadingIndex(null);
    }
  };

  useEffect(() => {
    const fetchAddress = async () => {
      if (!wallet) return;
      const address = await wallet.getChangeAddress();
      setAddress(address);
    };
    const confirmTransaction = async () => {
      if (txToConfirm) {
        const isConfirmed = await waitForTransactionConfirmation(txToConfirm);
        if (isConfirmed) {
          toast.success("Transaction confirmed!");
          dispatch(fetchRoadmaps());
        }
        setTxToConfirm(null); // Reset
      }
    };

    fetchAddress();
    confirmTransaction();
  }, [wallet, txToConfirm, dispatch]);

  if (loading) {
    return (
      <div className="flex justify-center items-center h-screen bg-gray-50">
        <Loader2
          className="animate-spin w-12 h-12 text-indigo-600"
          role="status"
        />
      </div>
    );
  }

  if (error) {
    return (
      <div className="flex justify-center items-center h-screen bg-gray-50">
        <div className="text-red-600 text-xl">{`Error: ${error}`}</div>
      </div>
    );
  }

  return (
    <div className="bg-gray-50 py-10 px-4 flex flex-wrap">
      {roadmaps.map((roadmap, index) => (
        <div
          key={index}
          className="max-w-2xl mx-auto bg-white shadow-lg rounded-2xl overflow-hidden p-6 md:flex md:gap-8 mb-10"
        >
          {/* Content */}
          <div className="flex flex-col justify-between">
            <div>
              <div className="flex items-center gap-2 mt-2 text-lg font-semibold text-indigo-600">
                <span>Ξ</span>
                <span>1 ADA = 1 P.C</span>
              </div>

              <h3 className="mt-6 text-xl font-semibold text-gray-700">
                Roadmap
              </h3>
              <ul className="list-disc list-inside text-gray-600 space-y-1 mt-2">
                <div key={roadmap.roadmapId} className="mb-2">
                  <h2 className="text-2xl font-bold text-gray-800">
                    {roadmap.roadmapName}
                  </h2>
                  <p className="font-medium">{roadmap.roadmapDescription}</p>
                  <ul className="ml-4 list-disc text-sm text-gray-500 space-y-1 mt-1">
                    <li>
                      Total Plastic Credits: {roadmap.totalPlasticCredits}
                    </li>
                    <li>Sold Plastic Credits: {roadmap.soldPlasticCredits}</li>
                    <li>Total Plastic Tokens: {roadmap.totalPlasticTokens}</li>
                    <li>Sent Plastic Tokens: {roadmap.sentPlasticTokens}</li>
                    <li>Total Plastic: {roadmap.totalPlastic}</li>
                    <li>Recovered Plastic: {roadmap.recoveredPlastic}</li>
                  </ul>
                </div>
              </ul>
            </div>

            {/* Buy Button */}
            <div className="my-6">
              <div className="mt-4">
                <label
                  htmlFor={`quantity-${index}`}
                  className="block text-sm font-medium text-gray-700"
                >
                  Enter ADA of Plastic Credit to Buy:
                </label>
                <input
                  id={`quantity-${index}`}
                  type="number"
                  min="1"
                  value={quantities[index] || 1}
                  onChange={(e) =>
                    setQuantities({
                      ...quantities,
                      [index]: Number(e.target.value),
                    })
                  }
                  className="mt-1 block w-full px-3 py-2 border border-gray-300 rounded-md shadow-sm focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm text-black"
                  placeholder="Enter amount"
                />
              </div>

              <button
                onClick={() => handleBuy(index)}
                disabled={loadingIndex === index}
                className="w-full bg-indigo-600 hover:bg-indigo-700 text-white font-semibold py-3 rounded-xl transition-all flex justify-center items-center mt-4"
                data-testid="buy-now-button"
              >
                {loadingIndex === index ? (
                  <>
                    <Loader2
                      className="animate-spin w-5 h-5 mr-2"
                      role="processing"
                    />
                    Processing...
                  </>
                ) : (
                  "Buy Now"
                )}
              </button>
            </div>
          </div>
        </div>
      ))}
    </div>
  );
};

export default NFTPurchase;
