// src/components/RoadmapDetails.tsx
import Button from "@/components/Button";
import Card from "./Card";
import CardContent from "./CardContent";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import { faArchive, faWallet } from "@fortawesome/free-solid-svg-icons";
import { Roadmap } from "@/pages/dashboard/types";
import { toast } from "sonner";
import axios from "axios";
import { decodeAddress, truncateAddress } from "@/utils/helper";
import { useEffect, useState } from "react";
import { Trust, User2 } from "@/assets/icons";
import { formatAmount } from "@/utils/helper";

interface RoadmapDetailsProps {
  roadmap: Roadmap;
  onBack: () => void;
}

const RoadmapDetails: React.FC<RoadmapDetailsProps> = ({ roadmap, onBack }) => {
  const handleRelease = async () => {
    try {
      if (roadmap.progress === 100) {
        const url = import.meta.env.VITE_SERVER_URL;
        const apiUrl = `${url}/roadmap/release`;
        const { data: res } = await axios.post(apiUrl, {
          roadmapId: roadmap.roadmapId,
          preId: roadmap.preId,
        });
        toast.success(res.message);
        onBack();
      } else {
        toast.error("Funds cannot be released until the progress is 100%");
      }
    } catch (error) {
      console.error("Error releasing funds:", error);
      toast.error("Failed to release funds. Please try again.");
    }
  };

  const [preAddress, setPreAddress] = useState(roadmap.preAddress ?? "");
  const [showAddAdmin, setShowAddAdmin] = useState(false);
  useEffect(() => {
    const fetchAddress = async () => {
      if (!preAddress || preAddress === "") {
        try {
          if (!roadmap.prePkh || !roadmap.preSkh) {
            console.error("Pre-preparation address not provided");
            return;
          }
          const decoded = await decodeAddress(roadmap.prePkh, roadmap.preSkh);
          setPreAddress(decoded);
        } catch (err) {
          console.error("Failed to decode address:", err);
        }
      }
    };

    fetchAddress();
  }, [preAddress, roadmap.preId]);

  // check if roadmap progresses is not 100% and it is not completed roadmap then return
  let completed = false;
  if (roadmap.progress !== 100 && !roadmap.preAddress) {
    completed = true;
  }

  return (
    <div className="flex flex-col gap-6">
      <div className="flex justify-between items-center">
        <div>
          <p className="text-md">Roadmap Details</p>
          <h1 className="text-2xl font-semibold">{roadmap.roadmapName}</h1>
          <div className="flex items-center text-[#525252] mt-1">
            <FontAwesomeIcon icon={faWallet} className="mr-2" />
            <p>{truncateAddress(preAddress)}</p>
          </div>
        </div>
        <div className="flex gap-2 mb-5">
          <button
            className="ml-auto flex items-center gap-1.5 bg-white text-[#0D0D0D] font-semibold px-4 py-2.5 rounded-full cursor-pointer  border border-[#0D0D0D]"
            onClick={() => setShowAddAdmin(true)}
          >
            <img src={User2} alt="user plus" className="w-5 h-5 invert-100" />
            <span>Add Admin</span>
          </button>
          <Button variant="outline" icon={faArchive}>
            Archive Roadmap
          </Button>
          <button
            className={`ml-auto flex items-center gap-1.5 font-semibold px-4 py-2.5 rounded-full cursor-pointer ${
              completed
                ? "bg-green-500 text-white border border-[#0D0D0D]"
                : "bg-[#B1B5B4] text-white"
            } hover:bg-gray}`}
          >
            <img src={Trust} alt="trust" className="w-5 h-5" />
            <span>Release Funds</span>
          </button>
        </div>
      </div>

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
            subtitle="(≈245,600 bottles)"
          />
        </Card>
        <Card>
          <CardContent
            title="PLASTIK tokens on hold"
            value={`${formatAmount(roadmap.sentPlasticTokens)} PLASTIK`}
            subtitle="(5,600 until fund release)"
          />
        </Card>
        <Card>
          <CardContent
            title="Funds Required"
            value={`${roadmap.totalPlasticCredits} USDM`}
            subtitle="25,000 USDM distributed"
          />
        </Card>
      </div>
    </div>
  );
};

export default RoadmapDetails;
