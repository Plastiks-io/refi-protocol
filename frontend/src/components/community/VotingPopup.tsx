import { faXmark } from "@fortawesome/free-solid-svg-icons";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import Button from "../Button";

interface VotingCardProps {
  votingPercentage: number;
  votingDuration: number;
  onStartVoting: () => void;
  onClose: () => void;
}

const VotingPopup = ({
  votingPercentage,
  votingDuration,
  onStartVoting,
  onClose,
}: VotingCardProps) => {
  const votingDate = new Date();
  votingDate.setDate(votingDate.getDate() + votingDuration);
  const votingEndDate = votingDate.toLocaleDateString("en-US", {
    day: "numeric",
    month: "long",
    year: "numeric",
  });

  return (
    <div className="fixed inset-0 bg-[rgba(0,0,0,0.7)] flex justify-center items-center z-50">
      <div className="bg-white text-gray-800 rounded-lg shadow-lg p-6 w-full max-w-md">
        {/* Header */}
        <div className="flex justify-between items-center pb-3">
          <h2 className="text-lg font-semibold">Create New Voting Round</h2>
          <button
            className="text-gray-500 hover:text-gray-800 cursor-pointer"
            onClick={onClose}
          >
            <FontAwesomeIcon icon={faXmark} className="w-5 h-5" />
          </button>
        </div>

        {/* Voting Details */}
        <div className="mt-4 space-y-4">
          <div>
            <p className="text-sm font-semibold">
              Selected Retirement Percentage
            </p>
            <div className="bg-black text-white text-center py-2 rounded-md text-lg font-bold">
              {votingPercentage}%
            </div>
          </div>
          <div>
            <p className="text-sm font-semibold">Voting Duration</p>
            <p className="text-gray-700 text-sm">
              This voting round will last for {votingDuration} days until{" "}
              {votingEndDate}
            </p>
          </div>
        </div>

        {/* Buttons */}
        <div className="flex justify-end gap-3 mt-6">
          <Button
            variant="userButton"
            className="bg-gray-200 text-gray-800 px-4 py-2 rounded-md text-lg"
            onClick={onClose}
          >
            Cancel
          </Button>
          <Button variant="dark" onClick={onStartVoting}>
            Create Voting Round
          </Button>
        </div>
      </div>
    </div>
  );
};

export default VotingPopup;
