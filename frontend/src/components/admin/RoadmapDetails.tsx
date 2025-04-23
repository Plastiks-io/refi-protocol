import { useNavigate, useParams } from "react-router-dom";
import Button from "../Button";
import Card from "./Card";
import CardContent from "./CardContent";
import { useSelector } from "react-redux";
import { RootState } from "../../redux/store";
import { toast } from "sonner";
import axios from "axios";

const RoadmapDetails = () => {
  const { roadmaps, loading, error } = useSelector(
    (state: RootState) => state.roadmaps
  );

  const { roadmapId } = useParams();
  const roadmap = roadmaps.find((r) => r.roadmapId === roadmapId);

  if (loading) return <div>Loading...</div>;
  if (error) return <div>Error loading roadmap</div>;
  if (!roadmap) return <div>Roadmap not found</div>;
  const Address =
    "addr_test1qregzqux7knjhg3v8npcp3t35w0dngwkz80ssgvywpk0ade9uy5qk6vl70ntchwh6qysnlww6q28vsjd6sz8kpdq2w0skcj8zp";
  const navigate = useNavigate();
  const handleRelease = async () => {
    try {
      // check if progress is 100%
      if (roadmap.progress === 100) {
        // release funds
        const url = import.meta.env.VITE_SERVER_URL;
        const apiUrl = `${url}/roadmap/release`;
        const { data: res } = await axios.post(apiUrl, {
          roadmapId: roadmap.roadmapId,
          preId: roadmap.preId,
        });
        toast.success(res.message);

        // after releasing funds, redirect back to admin page
        navigate("/admin");
      } else {
        toast.error("Funds cannot be released until the progress is 100%");
      }
    } catch (error) {
      console.error("Error releasing funds:", error);
      toast.error("Failed to release funds. Please try again.");
    }
  };
  return (
    <div className="mx-auto px-4 py-6 bg-white text-black md:px-10 lg:px-20 min-h-screen">
      <div className="flex justify-between items-center">
        <h1 className="text-xl font-bold">
          Roadmap Details: {roadmap.roadmapName}
        </h1>
        <div className="flex gap-2 mb-5">
          <Button variant="outline">Add Admin</Button>
          <Button variant="outline">Archive Roadmap</Button>
          <Button onClick={handleRelease}>Release Funds</Button>
        </div>
      </div>
      <p className="text-gray-500 text-sm">{Address}</p>
      <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
        <Card>
          <CardContent
            title="Total Plastic Credits Sold"
            value={roadmap.progress + "%"}
            progress={roadmap.progress}
          />
        </Card>
        <Card>
          <CardContent
            title="Plastic Recovered"
            value={roadmap.recoveredPlastic + " kg"}
            subtitle="(~245,000 bottles)"
          />
        </Card>
        <Card>
          <CardContent
            title="PLASTIK tokens on hold"
            value={roadmap.sentPlasticTokens + " PLASTIK"}
          />
        </Card>
        <Card>
          <CardContent
            title="Funds Required"
            value={roadmap.totalPlasticCredits + " USDM"}
            subtitle="25,000 USDM distributed"
          />
        </Card>
      </div>
    </div>
  );
};

export default RoadmapDetails;
