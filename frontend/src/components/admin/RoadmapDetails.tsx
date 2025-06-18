// src/components/RoadmapDetails.tsx
import Button from "@/components/Button";
import Card from "./Card";
import CardContent from "./CardContent";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import { faArchive, faWallet } from "@fortawesome/free-solid-svg-icons";
import { toast } from "sonner";
import axios from "axios";
import { formatDateTime, truncateAddress } from "@/utils/helper";
import { useContext, useState } from "react";
import { Trust, User2 } from "@/assets/icons";
import { formatAmount } from "@/utils/helper";
import { useParams, useNavigate } from "react-router-dom";
import { useDispatch, useSelector } from "react-redux";
import { AppDispatch, RootState } from "@/redux/store";
import { removeRoadmap, Roadmap } from "@/redux/roadmapSlice";
import { AlertTriangle, ArrowRight, Loader2, Recycle } from "lucide-react";
import AddAdminPopup from "./AddAdminPopup";
import { fetchArchivedRoadmaps } from "@/redux/archivedRoadmapSlice";
import { cardanoClient } from "@/services/cardano";
import { WalletContext } from "@/App";
import {
  addCompletedRoadmap,
  CompletedRoadmap,
} from "@/redux/completedRoadmapSlice";

const Transactions = [
  {
    id: "tx1",
    title: "Fund Transfer",
    subtitle: "10,000 USDM to Escrow",
    timestamp: new Date("2025-01-15T12:30:00Z"),
    type: "transfer",
  },
  {
    id: "tx2",
    title: "Credits Sold",
    subtitle: "5,000 kg of plastic",
    timestamp: new Date("2025-01-14T15:45:00Z"),
    type: "credits",
  },
];
const RoadmapDetails: React.FC = () => {
  const wallet = useContext(WalletContext);
  // Grab `roadmapId` from URL params
  const { roadmapId } = useParams<{ roadmapId: string }>();
  const navigate = useNavigate();
  const dispatch = useDispatch<AppDispatch>();

  // —— Select from “active” slice ——
  const {
    roadmaps: activeRoadmaps,
    loading: loadingActive,
    error: errorActive,
  } = useSelector((state: RootState) => state.roadmaps);

  // —— Select from “completed” slice ——
  const {
    roadmaps: completedRoadmaps,
    loading: loadingCompleted,
    error: errorCompleted,
  } = useSelector((state: RootState) => state.completedRoadmaps);

  // —— Select from “archived” slice ——
  const {
    roadmaps: archivedRoadmaps,
    loading: loadingArchived,
    error: errorArchived,
  } = useSelector((state: RootState) => state.archivedRoadmaps);

  // Aggregate loading flags and error messages
  const isLoading = loadingActive || loadingCompleted || loadingArchived;
  const errorMessage = errorActive || errorCompleted || errorArchived;

  // Merge all three arrays now that loading is false
  const allRoadmaps: Roadmap[] = [
    ...activeRoadmaps,
    ...completedRoadmaps,
    ...archivedRoadmaps,
  ];

  // Find the single roadmap matching `roadmapId`
  const roadmap = allRoadmaps.find((r) => r.roadmapId === roadmapId);

  const { admins: adminList } = useSelector((state: RootState) => state.admin);

  const { walletAddress } = useSelector((state: RootState) => state.wallet);
  const [showSendStablecoin, setShowSendStablecoin] = useState(false);
  const [sentAmount, setSentAmount] = useState("0");
  // Track “Add Admin” modal visibility
  const [showAddAdmin, setShowAddAdmin] = useState(false);

  // If no matching roadmap, show “not found”
  if (!roadmap) {
    return (
      <div className="flex justify-center items-center h-64">
        <p className="text-gray-600">Roadmap not found</p>
      </div>
    );
  }

  // Handler for “Release Funds” button
  const handleRelease = async () => {
    if (roadmap.progress !== 100 && Number(roadmap.fundsMissing) > 0) {
      toast.error(
        "Funds cannot be released until the progress is 100% or all funds are sent to escrow."
      );
      return;
    }

    if (!wallet) {
      toast.error("Wallet not connected.");
      return;
    }

    try {
      const txHash = await cardanoClient.releaseFunds(
        wallet,
        roadmap.preId,
        roadmap.roadmapId
      );
      toast.success(`Funds released successfully! Transaction Hash: ${txHash}`);
      const url = import.meta.env.VITE_SERVER_URL;
      const apiUrl = `${url}/roadmap/save`;
      const { data: res } = await axios.post(apiUrl, roadmap);
      toast.success(res.message);
      // After successful release, remove from active roadmaps
      dispatch(removeRoadmap(roadmap.roadmapId));
      // Also add in completed roadmaps
      const completedRoadmap: CompletedRoadmap = {
        id: res.roadmaps.id,
        ...roadmap,
      };
      dispatch(addCompletedRoadmap(completedRoadmap));
      navigate("/admin"); // go back to admin
    } catch (error) {
      console.error("Error releasing funds:", error);
      toast.error("Failed to release funds. Please try again.");
    }
  };

  const handleArchive = async () => {
    if (!wallet) {
      toast.error("Wallet not connected.");
      return;
    }
    try {
      console.log(roadmap);
      const txHash = await cardanoClient.archivedRoadmap(
        wallet,
        roadmap.preId,
        roadmap.roadmapId
      );
      toast.success(
        `Roadmap archived successfully! Transaction Hash: ${txHash}`
      );
      const url = import.meta.env.VITE_SERVER_URL;
      const apiUrl = `${url}/roadmap/archive`;

      const { data: res } = await axios.post(apiUrl, roadmap, {
        withCredentials: true,
      });
      // after successful archiving again fetch archived roadmaps
      dispatch(fetchArchivedRoadmaps());
      // remove from active roadmaps
      dispatch(removeRoadmap(roadmap.roadmapId));
      toast.success(res.message);
      navigate("/admin"); // go back one page
    } catch (error) {
      console.error("Error archiving roadmap:", error);
      toast.error("Failed to archive roadmap. Please try again.");
    }
  };

  const sendStablecoinsToEscrow = async () => {
    if (!wallet) {
      toast.error("Wallet not connected.");
      return;
    }
    if (!sentAmount || Number(sentAmount) <= 0) {
      toast.error("Please enter a valid amount to send.");
      return;
    }
    if (Number(sentAmount) > Number(roadmap.fundsMissing)) {
      toast.error(
        `You can only send up to ${
          Number(roadmap.fundsMissing) / 1_000_000
        } USDM`
      );
      return;
    }
    try {
      setShowSendStablecoin(false);
      const txHash = await cardanoClient.fundUSDM(
        wallet,
        roadmap.preId,
        roadmap.roadmapId,
        BigInt(sentAmount)
      );
      toast.success("Stablecoins sent to escrow successfully! " + txHash);
      console.log("Transaction Hash:", txHash);
    } catch (error) {
      console.error("Error sending stablecoins to escrow:", error);
      toast.error(
        error instanceof Error ? error.message : "Failed to send stablecoins"
      );
    }
  };

  const disabled =
    roadmap.status === "completed" ||
    roadmap.status === "archived" ||
    roadmap.progress !== 100 ||
    Number(roadmap.fundsMissing) > 0;

  // ── Render ──
  return isLoading ? (
    <div className="flex justify-center items-center h-64">
      <Loader2 className="animate-spin" />
    </div>
  ) : errorMessage ? (
    <div>
      <p className="text-gray-600">
        Failed to load roadmap details. Please try again.
      </p>
    </div>
  ) : (
    <div className="bg-white text-black mx-auto px-4 md:px-10 lg:px-20 py-6 min-h-screen">
      {/* ── Header ── */}
      <div className="flex justify-between items-center">
        <div>
          <p className="text-md">Roadmap Details</p>
          <h1 className="text-2xl font-semibold">{roadmap.roadmapName}</h1>

          <div className="flex items-center text-[#525252] my-2">
            <FontAwesomeIcon icon={faWallet} className="mr-2" />
            <p>{truncateAddress(roadmap.preAddress)}</p>
          </div>
        </div>

        <div className="flex gap-2 mb-5">
          {/* Add Admin button */}
          <button
            className="ml-auto flex items-center gap-1.5 bg-white text-[#0D0D0D] font-semibold px-4 py-2.5 rounded-full cursor-pointer border border-[#0D0D0D]"
            onClick={() => setShowAddAdmin(true)}
          >
            <img src={User2} alt="user plus" className="w-5 h-5 invert-100" />
            <span>Add Admin</span>
          </button>

          {/* Archive Roadmap button */}
          <Button variant="outline" icon={faArchive} onClick={handleArchive}>
            Archive Roadmap
          </Button>

          {/* Release Funds button */}
          <button
            onClick={handleRelease}
            className={`ml-auto flex items-center gap-1.5 font-semibold px-4 py-2.5 rounded-full cursor-pointer ${
              !disabled
                ? "bg-green-500 text-white border border-[#0D0D0D]"
                : "bg-[#B1B5B4] text-white"
            } hover:bg-gray`}
            disabled={disabled}
          >
            <img src={Trust} alt="trust" className="w-5 h-5" />
            <span>Release Funds</span>
          </button>
        </div>
      </div>

      {/* ── Stats Cards ── */}
      <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
        <Card>
          <CardContent
            title="Total Plastic Credits Sold"
            value={`${roadmap.progress}%`}
            progress={roadmap.progress}
          />
        </Card>

        <Card>
          <CardContent
            title="Plastic Recovered"
            value={`${roadmap.recoveredPlastic} kg`}
            subtitle={`≈(${roadmap.recoveredPlastic * 56} bottles)`}
          />
        </Card>

        <Card>
          <CardContent
            title="PLASTIK tokens on hold"
            value={`${formatAmount(roadmap.sentPlasticTokens)} PLASTIK`}
            subtitle={`(${
              roadmap.totalPlasticTokens - roadmap.sentPlasticTokens
            } until fund release)`}
          />
        </Card>

        <Card>
          <CardContent
            title="Funds Required"
            value={`${roadmap.totalPlasticCredits} USDM`}
            subtitle={`${roadmap.soldPlasticCredits} USDM distributed`}
          />
        </Card>
      </div>

      {/* Escrow fund Status */}
      <div className="bg-white rounded-2xl shadow-md p-6 w-full mx-auto border border-[#E5E7EB]">
        {/* ── Header ── */}
        <div className="flex items-center justify-between mb-4">
          <h2 className="text-2xl font-semibold text-gray-900">
            Escrow Status
          </h2>

          {Number(roadmap.fundsMissing) > 0 ? (
            <span className="flex items-center gap-1 bg-[#FFDC85] text-[#1B1B1F] text-sm font-medium px-3 py-1 rounded-full">
              <AlertTriangle className="w-4 h-4" />
              Pending Funds
            </span>
          ) : (
            <span className="flex items-center gap-1 bg-green-100 text-green-800 text-sm font-medium px-3 py-1 rounded-full">
              <Recycle className="w-4 h-4" />
              Funds Released
            </span>
          )}
        </div>

        {/* ── Total Funds Distributed ── */}
        <div className="mb-4">
          <p className="text-sm text-gray-600">Total Funds Distributed</p>
          <p className="mt-1 text-2xl font-bold text-gray-900">
            {Number(roadmap.fundsDistributed) / 1_000_000} USDM
          </p>
        </div>

        <hr className="border-1 border-[#C9C9C9] my-4" />

        {/* ── Amount Pending Row ── */}
        {Number(roadmap.fundsMissing) > 0 && (
          <div className="flex flex-col md:flex-row md:items-center md:justify-between mb-6">
            {/* Left: Amount Pending */}
            <div className="md:w-1/2">
              <p className="text-sm text-gray-600">
                Amount Pending to be Sent to Escrow
              </p>
              <p className="mt-1 text-xl font-bold text-red-600">
                {Number(roadmap.fundsMissing) / 1_000_000} USDM
              </p>
            </div>

            {/* Right: Send Stablecoins Button */}
            <div className="mt-4 md:mt-0 md:w-1/2 flex justify-start md:justify-end">
              <button
                onClick={() => setShowSendStablecoin(true)}
                className="bg-[#082FB9] hover:bg-blue-700 text-white font-semibold px-5 py-2.5 rounded-full transition"
              >
                Send Stablecoins to Escrow
              </button>
            </div>
          </div>
        )}
      </div>
      {/*  showSendStablecoin Modal */}
      {showSendStablecoin && (
        <div className="fixed inset-0 bg-[rgba(0,0,0,0.5)] bg-opacity-50 flex items-center justify-center p-4">
          <div className="bg-white rounded-xl p-6 w-full max-w-lg py-10">
            <div className="flex items-center mb-4">
              <h3 className="text-xl font-semibold">Send USDM Tokens</h3>
            </div>
            <input
              type="number"
              step="0.000001"
              value={(Number(sentAmount) / 1_000_000).toFixed(6)}
              onChange={(e) => {
                // parse the USDM decimal the user typed
                const usdm = parseFloat(e.target.value) || 0;
                // convert to micro and store
                setSentAmount(Math.floor(usdm * 1_000_000).toString());
              }}
              placeholder="Enter amount to sent to escrow"
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
                    // total missing in micro‑USDM
                    const totalMicro = Number(roadmap.fundsMissing);
                    // take percent of that, floor to integer
                    const pctMicro = Math.floor((totalMicro * percent) / 100);
                    setSentAmount(pctMicro.toString());
                  }}
                >
                  {percent}%
                </button>
              ))}
            </div>

            <div className="flex gap-4">
              <button
                onClick={() => setShowSendStablecoin(false)}
                className="flex-1 px-4 py-2 bg-white rounded-2xl shadow-md border border-gray-200 hover:bg-gray-50"
              >
                Cancel
              </button>
              <button
                onClick={sendStablecoinsToEscrow}
                className="flex-1 px-4 py-2 bg-green-600 text-white hover:bg-green-700 rounded-2xl shadow-md border border-gray-200"
              >
                Confirm
              </button>
            </div>
          </div>
        </div>
      )}

      {/* Transaction History of fund transfered */}
      <div className="w-full mx-auto mt-5">
        {/* ── Header ── */}
        <div className="mb-2">
          <h2 className="text-2xl font-semibold text-gray-900">
            Transaction History
          </h2>

          <p className="mt-1 text-sm text-gray-600">
            Below is the transaction details of usdm transfered
          </p>
        </div>

        {/* ── Card Container ── */}
        <div className="bg-white rounded-xl shadow-md overflow-hidden border border-[#E5E7EB]">
          {Transactions.map((tx, idx) => {
            const { datePart, timePart } = formatDateTime(tx.timestamp);
            const isLast = idx === Transactions.length - 1;

            return (
              <div
                key={tx.id}
                className={`flex items-center justify-between px-4 py-4 sm:px-6 ${
                  !isLast ? "border-b border-gray-200" : ""
                }`}
              >
                {/* Left section: icon + title/subtitle */}
                <div className="flex items-center">
                  <div className="flex-shrink-0 bg-[#082FB9] text-white rounded-full p-2">
                    {tx.type === "transfer" ? <ArrowRight /> : <Recycle />}
                  </div>
                  <div className="ml-4">
                    <p className="text-base font-medium text-gray-900">
                      {tx.title}
                    </p>
                    <p className="mt-1 text-sm text-gray-600">{tx.subtitle}</p>
                  </div>
                </div>

                {/* Right section: date/time */}
                <div className="flex flex-col items-end">
                  <p className="text-sm font-medium text-gray-900">
                    {datePart}
                  </p>
                  <p className="mt-1 text-xs text-gray-500">{timePart}</p>
                </div>
              </div>
            );
          })}
        </div>
      </div>
      <AddAdminPopup
        isOpen={showAddAdmin}
        onClose={() => setShowAddAdmin(false)}
        existingAdmins={adminList}
        walletAddress={walletAddress}
      />
    </div>
  );
};

export default RoadmapDetails;
