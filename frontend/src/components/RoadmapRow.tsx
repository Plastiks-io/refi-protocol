// src/components/RoadmapRow.tsx
import React from "react";
import { TriangleAlert } from "lucide-react";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import { faCircleCheck } from "@fortawesome/free-regular-svg-icons";
import { Roadmap } from "@/redux/roadmapSlice";
import { useNavigate } from "react-router-dom";

const RoadmapRow: React.FC<Roadmap> = ({
  preId,
  roadmapId,
  roadmapName,
  progress,
  totalPlasticCredits,
  totalPlasticTokens,
}) => {
  const date = new Date().toLocaleDateString("en-US", {
    year: "numeric",
    month: "short",
    day: "2-digit",
  });

  const isPending = false; // or derive from props

  const navigate = useNavigate();

  return (
    <tr className="border-b border-gray-200">
      <td className="p-4 whitespace-nowrap">{preId}</td>
      <td className="p-4 whitespace-nowrap">{roadmapName}</td>
      <td className="p-4 whitespace-nowrap">{date}</td>
      <td className="p-4 whitespace-nowrap">
        <div className="w-24 h-2 bg-gray-300 rounded-full">
          <div
            className="h-full bg-[#082FB9] rounded-full"
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
      <td className="p-4 whitespace-nowrap">
        {isPending ? (
          <div className="flex items-center justify-center rounded-full gap-2 p-2 bg-[#FFDC85]">
            <TriangleAlert />
            <span>Pending</span>
          </div>
        ) : (
          <div className="flex items-center justify-center rounded-full gap-2 p-2 bg-[#A7FE8A]">
            <FontAwesomeIcon icon={faCircleCheck} />
            <span>Up to date</span>
          </div>
        )}
      </td>
      <td className="p-4">
        <button
          className="text-[#082FB9] border-b-2 hover:text-blue-600 transition cursor-pointer"
          onClick={() => {
            // Navigate to details page
            // Example: navigate(`/roadmap/${preId}`);
            navigate(`/admin/${roadmapId}`);
          }}
        >
          View Details
        </button>
      </td>
    </tr>
  );
};

export default RoadmapRow;
