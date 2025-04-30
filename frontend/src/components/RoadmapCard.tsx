import Button from "./Button";
import { Roadmap } from "../pages/dashboard/types";

const RoadmapCard: React.FC<Roadmap> = ({
  roadmapName,
  roadmapDescription,
  progress,
  totalPlasticCredits,
  soldPlasticCredits,
  totalPlasticTokens,
  sentPlasticTokens,
  totalPlastic,
  recoveredPlastic,
}) => {
  return (
    <div className="bg-white p-4 sm:p-6 text-black rounded-lg shadow-md mb-4 w-full">
      {/* Title Section */}
      <div className="mt-2 sm:mt-4 flex flex-wrap justify-between items-center">
        <div className="w-full sm:w-auto">
          <h3 className="text-lg sm:text-xl font-bold">{roadmapName}</h3>
          <p className="text-gray-600 text-sm sm:text-base">
            {roadmapDescription}
          </p>
        </div>
        {progress < 100 ? (
          <span className="bg-gray-200 text-gray-600 px-3 py-1 rounded-full text-xs sm:text-sm mt-2 sm:mt-0">
            In Progress
          </span>
        ) : (
          <span className="bg-gray-200 text-gray-600 px-3 py-1 rounded-full text-xs sm:text-sm mt-2 sm:mt-0">
            Completed
          </span>
        )}
      </div>

      {/* Progress Section */}
      <div className="mt-4">
        <div className="flex justify-between items-center text-sm sm:text-base">
          <span>Progress</span>
          <span>{progress}%</span>
        </div>
        <div
          className="w-full bg-gray-200 h-2 rounded-full mt-2"
          role="progressbar"
        >
          <div
            className="bg-black h-2 rounded-full"
            style={{ width: `${progress}%` }}
          ></div>
        </div>
      </div>

      {/* Details Section */}
      <div className="flex flex-col sm:flex-row justify-between items-start sm:items-end mt-4 gap-3">
        <div className="text-sm sm:text-base">
          <p>
            Kg of Plastic: {recoveredPlastic}/{totalPlastic} kg
          </p>
          <p>
            Money in custody: ${soldPlasticCredits}/{totalPlasticCredits}
          </p>
          <p>
            PLASTIK Tokens: {sentPlasticTokens}/{totalPlasticTokens}
          </p>
        </div>
        <Button variant="outline" className="w-full sm:w-auto">
          View Details
        </Button>
      </div>
    </div>
  );
};

export default RoadmapCard;
