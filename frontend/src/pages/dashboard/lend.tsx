import { useContext, useEffect, useState } from "react";
import { DollarSign, Info, Loader2 } from "lucide-react";
import { WalletContext } from "@/App";
import { cardanoClient } from "@/services/cardano";
import { toast } from "sonner";
import { useSelector } from "react-redux";
import { RootState } from "@/redux/store";
import { useCardanoData } from "@/contexts/cardanoContexts";

export default function Lend() {
  // on‑chain values
  const { data, refresh } = useCardanoData();
  // local UI state
  const [showWithdrawalWarning, setShowWithdrawalWarning] = useState(false);
  const [showLendModal, setShowLendModal] = useState(false);
  const [lendAmount, setLendAmount] = useState("");
  const [withdrawAmount, setWithdrawAmount] = useState("");
  const [plastikHoldings, setPlastikHoldings] = useState(0);

  const wallet = useContext(WalletContext);
  const { roadmaps } = useSelector((state: RootState) => state.roadmaps);

  useEffect(() => {
    if (wallet === null) {
      setPlastikHoldings(0);
      return;
    }
    fetchBalances();
  }, [wallet]);

  const handleLendTokens = async () => {
    const amount = parseInt(lendAmount, 10);
    if (amount <= 0) return;

    try {
      if (!wallet) throw new Error("Wallet not connected");
      const txHash = await cardanoClient.depositPlastik(wallet, BigInt(amount));
      toast.success("Tokens lent successfully! " + txHash, {
        closeButton: true,
      });
      setShowLendModal(false);
      setLendAmount("");
    } catch (error) {
      console.error("Error lending tokens:", error);
      toast.error(
        `Failed to lend tokens: ${
          error instanceof Error ? error.message : "Unknown error"
        }`,
        {
          closeButton: true,
        }
      );
    }
  };

  const handleWithdraw = async () => {
    const amount = parseInt(withdrawAmount, 10);
    if (amount <= 0) return;

    if (data && BigInt(amount) > data.staked) {
      setShowWithdrawalWarning(true);
      return;
    }

    try {
      if (!wallet) throw new Error("Wallet not connected");
      const txHash = await cardanoClient.withdrawPlastik(
        wallet,
        BigInt(amount)
      );
      toast.success("Tokens withdrawn successfully! " + txHash, {
        closeButton: true,
      });
      setShowWithdrawalWarning(false);
      setWithdrawAmount("");
    } catch (error) {
      console.error("Error withdrawing tokens:", error);
      toast.error(
        `Failed to withdraw tokens: ${
          error instanceof Error ? error.message : "Unknown error"
        }`,
        {
          closeButton: true,
        }
      );
    }
  };

  const redeemReward = async () => {
    if (!data || data.rewardDebt === BigInt(0)) {
      toast.error("No rewards available to redeem", {
        closeButton: true,
      });
      return;
    }
    try {
      if (!wallet) throw new Error("Wallet not connected");
      const txHash = await cardanoClient.redeemReward(wallet);
      toast.success("Tokens withdrawn successfully! " + txHash, {
        closeButton: true,
      });
    } catch (error) {
      console.error(error);
      toast.error(
        `${error instanceof Error ? error.message : "Unknown error"}`,
        {
          closeButton: true,
        }
      );
    }
  };

  const fetchBalances = async () => {
    try {
      const plastikTokenAddress = import.meta.env.VITE_PLASTIC_TOKEN;

      if (!wallet) return;
      const balances = await wallet.getBalance();
      console.log(balances);
      console.log("plastikTokenAddress", plastikTokenAddress);

      const plastik = balances.find(
        (item) => item.unit === plastikTokenAddress
      )?.quantity;
      console.log(plastik);
      setPlastikHoldings(plastik ? Number(plastik) : 0);
    } catch (error) {
      console.error("Error fetching balances:", error);
      toast.error(
        `Failed to fetch balances: ${
          error instanceof Error ? error.message : "Unknown error"
        }`,
        {
          closeButton: true,
        }
      );
    }
  };

  if (data === null) {
    return (
      <div
        className="bg-white flex justify-center items-center min-h-screen"
        role="status"
      >
        <Loader2 className="animate-spin w-10 h-10 text-gray-600" />
      </div>
    );
  }

  const stakedNumber = Number(data.staked);
  const rewardNumber = Number(data.rewardDebt);

  return (
    <div className="bg-white text-black mx-auto px-4 md:px-10 lg:px-20 py-6 min-h-screen">
      {/* Lending Header */}
      <div className="bg-[#e7eaf7] p-6 rounded-xl mb-8">
        <h1 className="text-2xl font-bold text-[#082FB9] mb-2">
          Put your PLASTIK tokens to use
        </h1>
        <p className="text-[#082FB9]">
          Lend up to 1,000,000 PLASTIK and earn 2% interest when plastik credit
          is sold.
        </p>
      </div>
      {/* Stats Grid */}
      <div className="grid grid-cols-2 md:grid-cols-4 gap-4 mb-8">
        <div className="bg-white rounded-2xl shadow-md border border-gray-200 p-4">
          <h2 className="text-gray-500 text-sm mb-1">PLASTIK Lent</h2>
          <p className="text-2xl font-bold">{stakedNumber.toLocaleString()}</p>
        </div>
        <div className="bg-white rounded-2xl shadow-md border border-gray-200 p-4">
          <h2 className="text-gray-500 text-sm mb-1">Reward USDM</h2>
          <p className="text-2xl font-bold">
            {Number(rewardNumber / 1_000_000).toLocaleString(undefined, {
              minimumFractionDigits: 6,
              maximumFractionDigits: 6,
            })}
          </p>
        </div>
        <div className="bg-white rounded-2xl shadow-md border border-gray-200 p-4 flex items-center justify-center">
          <button
            onClick={() => setShowWithdrawalWarning(true)}
            className="bg-blue-600 hover:bg-blue-700 text-white font-semibold px-5 py-2.5 rounded-full"
          >
            Withdraw
          </button>
        </div>
        <div className="bg-white rounded-2xl shadow-md border border-gray-200 p-4 flex items-center justify-center">
          <button
            onClick={redeemReward}
            className="bg-blue-600 hover:bg-blue-700 text-white font-semibold px-5 py-2.5 rounded-full"
          >
            Redeem Rewards
          </button>
        </div>
      </div>

      {/* Roadmap Allocation Table */}
      <div className="bg-white rounded-xl shadow-sm overflow-hidden">
        <div className="p-6 border-b">
          <h2 className="text-lg font-semibold">Active Allocations</h2>
        </div>
        <div className="overflow-x-auto">
          <table className="w-full">
            <thead className="bg-gray-50">
              <tr>
                <th className="px-6 py-3 text-left text-sm font-medium text-gray-500">
                  Roadmap
                </th>
                <th className="px-6 py-3 text-right text-sm font-medium text-gray-500">
                  Tokens Lent
                </th>
                <th className="px-6 py-3 text-right text-sm font-medium text-gray-500">
                  Progress
                </th>
                <th className="px-6 py-3 text-right text-sm font-medium text-gray-500">
                  Reward
                </th>
              </tr>
            </thead>
            <tbody className="divide-y divide-gray-200">
              {roadmaps.map((roadmap, index) => (
                <tr key={index}>
                  <td className="px-6 py-4 text-sm font-medium text-gray-900">
                    {roadmap.roadmapName}
                  </td>
                  <td className="px-6 py-4 text-right text-sm text-gray-500">
                    {roadmap.sentPlasticTokens.toLocaleString()} PLASTIK
                  </td>
                  <td className="px-6 py-4 text-right">
                    <div className="flex items-center justify-end">
                      <div className="w-32 h-2 bg-gray-200 rounded-full overflow-hidden">
                        <div
                          className="h-full bg-[#082FB9] transition-all"
                          style={{ width: `${roadmap.progress}%` }}
                        />
                      </div>
                      <span className="ml-2 text-sm text-gray-500">
                        {roadmap.progress}%
                      </span>
                    </div>
                  </td>
                  <td className="px-6 py-4 text-right text-sm text-gray-500">
                    <div className="flex items-center justify-end">
                      <DollarSign className="w-4 h-4 mr-1 text-[#082FB9]" />
                      {(roadmap.soldPlasticCredits * 2) / 100} USDM
                    </div>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </div>

      {/* Lend Tokens Modal */}
      {showLendModal && (
        <div className="fixed inset-0 bg-[rgba(0,0,0,0.5)] flex items-center justify-center p-4">
          <div className="bg-white rounded-xl p-6 w-full max-w-lg py-10">
            <h3 className="text-xl font-semibold mb-4">Lend PLASTIK Tokens</h3>
            <input
              type="number"
              value={lendAmount}
              onChange={(e) => setLendAmount(e.target.value)}
              placeholder="Enter amount to lend"
              className="w-full p-3 bg-white rounded-2xl shadow-md border border-gray-200 mb-4"
            />
            {/* Percentage Buttons */}
            <div className="flex gap-2 mb-4">
              {[25, 50, 75, 100].map((percent) => (
                <button
                  key={percent}
                  type="button"
                  className="flex-1 px-2 py-1 bg-blue-100 text-blue-700 rounded-full font-medium hover:bg-blue-200 transition"
                  onClick={() => {
                    if (data) {
                      const value = Math.floor(
                        Number(plastikHoldings) * (percent / 100)
                      );
                      setLendAmount(value.toString());
                    }
                  }}
                >
                  {percent}%
                </button>
              ))}
            </div>
            <p className="text-lg font-semibold text-gray-500 mb-4">
              Total Plastik Holdings: {plastikHoldings.toLocaleString()}
            </p>
            <div className="flex gap-4">
              <button
                onClick={() => setShowLendModal(false)}
                className="flex-1 px-4 py-2 bg-white rounded-2xl shadow-md border border-gray-200"
              >
                Cancel
              </button>
              <button
                onClick={handleLendTokens}
                className="flex-1 px-4 py-2 bg-green-600 text-white hover:bg-green-700 rounded-2xl shadow-md border border-gray-200"
              >
                Confirm
              </button>
            </div>
          </div>
        </div>
      )}

      {/*  Withdrawal Warning Modal */}
      {showWithdrawalWarning && (
        <div className="fixed inset-0 bg-[rgba(0,0,0,0.5)] bg-opacity-50 flex items-center justify-center p-4">
          <div className="bg-white rounded-xl p-6 w-full max-w-lg py-10">
            <div className="flex items-center mb-4">
              <Info className="w-6 h-6 text-yellow-500 mr-2" />
              <h3 className="text-xl font-semibold">Withdraw PLASTIK Tokens</h3>
            </div>
            <input
              type="number"
              value={withdrawAmount}
              onChange={(e) => setWithdrawAmount(e.target.value)}
              placeholder="Enter amount to withdraw"
              className="w-full p-3 bg-white rounded-2xl shadow-md border border-gray-200 mb-4"
            />
            {/* Percentage Buttons */}
            <div className="flex gap-2 mb-4">
              {[25, 50, 75, 100].map((percent) => (
                <button
                  key={percent}
                  type="button"
                  className="flex-1 px-2 py-1 bg-blue-100 text-blue-700 rounded-full font-medium hover:bg-blue-200 transition"
                  onClick={() => {
                    if (data) {
                      const value = Math.floor(
                        Number(data.staked) * (percent / 100)
                      );
                      setWithdrawAmount(value.toString());
                    }
                  }}
                >
                  {percent}%
                </button>
              ))}
            </div>
            <p className="text-lg font-semibold text-gray-500 mb-4">
              Total Plastik Lent:{" "}
              {data ? data.staked.toLocaleString() : "Loading..."}
            </p>
            <div className="flex gap-4">
              <button
                onClick={() => setShowWithdrawalWarning(false)}
                className="flex-1 px-4 py-2 bg-white rounded-2xl shadow-md border border-gray-200 hover:bg-gray-50"
              >
                Cancel
              </button>
              <button
                onClick={handleWithdraw}
                className="flex-1 px-4 py-2 bg-green-600 text-white hover:bg-green-700 rounded-2xl shadow-md border border-gray-200"
              >
                Confirm
              </button>
            </div>
          </div>
        </div>
      )}

      {/* Floating Lend Button */}
      <div className="flex justify-center mt-6">
        <button
          className="bg-blue-600 hover:bg-blue-700 text-white font-semibold px-5 py-2.5 rounded-full"
          onClick={() => setShowLendModal(true)}
        >
          Lend Tokens
        </button>
      </div>
    </div>
  );
}
