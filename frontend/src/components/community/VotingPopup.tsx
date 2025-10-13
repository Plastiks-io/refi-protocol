import { faXmark } from "@fortawesome/free-solid-svg-icons";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import Button from "../Button";

interface VotingCardProps {
  votingPercentage: number;
  votingDuration: number;
  onStartVoting: () => void;
  onClose: () => void;
  isCreating?: boolean;
}

const VotingPopup = ({
  votingPercentage,
  votingDuration,
  onStartVoting,
  onClose,
  isCreating = false,
}: VotingCardProps) => {
  const votingDate = new Date();
  votingDate.setDate(votingDate.getDate() + votingDuration);
  const votingEndDate = votingDate.toLocaleDateString("en-US", {
    day: "numeric",
    month: "long",
    year: "numeric",
  });

  const votingEndTime = votingDate.toLocaleTimeString("en-US", {
    hour: "2-digit",
    minute: "2-digit",
  });

  // Prevent closing while creating
  const handleClose = () => {
    if (!isCreating) {
      onClose();
    }
  };

  // Prevent background click while creating
  const handleBackgroundClick = (e: React.MouseEvent<HTMLDivElement>) => {
    if (e.target === e.currentTarget && !isCreating) {
      onClose();
    }
  };

  return (
    <div
      className="fixed inset-0 bg-[rgba(0,0,0,0.7)] flex justify-center items-center z-50 p-4"
      onClick={handleBackgroundClick}
    >
      <div className="bg-white text-gray-800 rounded-2xl shadow-2xl p-6 w-full max-w-lg animate-fadeIn">
        {/* Header */}
        <div className="flex flex-col">
          <div className="w-full flex justify-between items-center mb-4">
            <div className="w-6"></div> {/* Spacer for centering */}
            <h2 className="text-2xl font-semibold text-[#0D0D0D] flex-1 text-center">
              Create New Voting Round
            </h2>
            <button
              className={`text-gray-500 hover:text-gray-700 transition-colors ${
                isCreating ? "cursor-not-allowed opacity-50" : "cursor-pointer"
              }`}
              onClick={handleClose}
              disabled={isCreating}
              aria-label="Close"
            >
              <FontAwesomeIcon icon={faXmark} className="w-6 h-6" />
            </button>
          </div>
        </div>

        {/* Voting Details */}
        <div className="mt-6 space-y-5">
          {/* Selected Percentage */}
          <div className="mb-4">
            <p className="text-base font-semibold mb-3 text-gray-700">
              Selected Retirement Percentage
            </p>
            <div className="bg-gradient-to-r from-[#082FB9] to-[#0a3dd6] text-white text-center py-4 rounded-lg text-3xl font-bold shadow-md">
              {votingPercentage}%
            </div>
            <p className="text-xs text-gray-500 mt-2 text-center">
              This will be the proposed new token retirement rate
            </p>
          </div>

          {/* Voting Duration */}
          <div className="bg-gray-50 p-4 rounded-lg border border-gray-200">
            <p className="text-base font-semibold mb-2 text-gray-700">
              Voting Period
            </p>
            <div className="space-y-1">
              <p className="text-sm text-gray-600">
                <span className="font-medium">Duration:</span> {votingDuration}{" "}
                days
              </p>
              <p className="text-sm text-gray-600">
                <span className="font-medium">Ends on:</span> {votingEndDate} at{" "}
                {votingEndTime}
              </p>
            </div>
          </div>

          {/* Warning Message */}
          {isCreating && (
            <div className="bg-yellow-50 border border-yellow-200 p-3 rounded-lg">
              <p className="text-sm text-yellow-800 font-medium text-center">
                ⏳ Creating proposal on blockchain... Please wait and do not
                close this window.
              </p>
            </div>
          )}
        </div>

        {/* Buttons */}
        <div className="flex justify-center items-center gap-3 mt-8">
          <Button
            variant="userButton"
            className={`bg-white border-2 border-gray-300 text-gray-700 px-6 py-2.5 rounded-full text-base font-semibold hover:bg-gray-50 transition-all ${
              isCreating
                ? "opacity-50 cursor-not-allowed"
                : "hover:border-gray-400"
            }`}
            onClick={handleClose}
            disabled={isCreating}
          >
            Cancel
          </Button>
          <Button
            variant="userButton"
            onClick={onStartVoting}
            disabled={isCreating}
            className={`rounded-full text-white px-6 py-2.5 text-base font-semibold transition-all ${
              isCreating
                ? "bg-gray-400 cursor-not-allowed"
                : "bg-[#082FB9] hover:bg-[#0624a3] shadow-md hover:shadow-lg"
            }`}
          >
            {isCreating ? (
              <span className="flex items-center gap-2">
                <svg
                  className="animate-spin h-5 w-5 text-white"
                  xmlns="http://www.w3.org/2000/svg"
                  fill="none"
                  viewBox="0 0 24 24"
                >
                  <circle
                    className="opacity-25"
                    cx="12"
                    cy="12"
                    r="10"
                    stroke="currentColor"
                    strokeWidth="4"
                  ></circle>
                  <path
                    className="opacity-75"
                    fill="currentColor"
                    d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
                  ></path>
                </svg>
                Creating Proposal...
              </span>
            ) : (
              "Create Voting Round"
            )}
          </Button>
        </div>

        {/* Additional Info */}
        <div className="mt-6 pt-4 border-t border-gray-200">
          <p className="text-xs text-gray-500 text-center">
            By creating this proposal, you agree to the governance rules and
            understand that this action requires a blockchain transaction.
          </p>
        </div>
      </div>
    </div>
  );
};

export default VotingPopup;
