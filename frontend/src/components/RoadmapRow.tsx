import React from "react";
import Button from "./Button";
import { Roadmap } from "../pages/dashboard/types";
import { useNavigate } from "react-router-dom";

const RoadmapRow: React.FC<Roadmap> = ({
  preId,
  roadmapName,
  progress,
  totalPlasticCredits,
  totalPlasticTokens,
  roadmapId,
}) => {
  const date = new Date().toLocaleDateString("en-US", {
    year: "numeric",
    month: "short",
    day: "2-digit",
  });

  const navigate = useNavigate();
  const handleClick = () => {
    navigate(`/admin/roadmap/${roadmapId}`);
  };
  return (
    <tr className="border-b border-gray-200">
      <td className="p-4 whitespace-nowrap">{preId}</td>
      <td className="p-4 whitespace-nowrap">{roadmapName}</td>
      <td className="p-4 whitespace-nowrap">{date}</td>
      <td className="p-4 whitespace-nowrap">
        <div className="w-24 h-2 bg-gray-300 rounded-full">
          <div
            className="h-full bg-black rounded-full"
            style={{ width: `${progress}%` }}
          ></div>
        </div>
      </td>
      <td className="p-4 whitespace-nowrap">
        {totalPlasticCredits.toLocaleString("en-US")} USDM
      </td>
      <td className="p-4 whitespace-nowrap">
        {totalPlasticTokens.toLocaleString("en-US")} PLASTIK
      </td>
      <td className="p-4">
        <Button onClick={handleClick}>Manage Roadmap</Button>
      </td>
    </tr>
  );
};

export default RoadmapRow;
