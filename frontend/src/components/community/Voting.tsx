import { useState, useEffect } from "react";
import Button from "../Button";

interface VotingRetirementProps {
  eligibleforVoting: boolean;
  oldRetirementRate: number;
  newRetirementRate: number;
  oldRetirementPercentage: number;
  newRetirementPercentage: number;
  hasVoted: boolean;
  onVote: (inFavor: boolean) => Promise<void>;
  isVoting: boolean;
  votesFor: number;
  votesAgainst: number;
  countdown: string;
}

const Voting = ({
  eligibleforVoting,
  oldRetirementRate,
  newRetirementRate,
  hasVoted,
  onVote,
  isVoting,
  votesFor,
  votesAgainst,
  countdown,
}: VotingRetirementProps) => {
  const [selectedOption, setSelectedOption] = useState<string | null>(null);
  const minimumPlastikToVote = import.meta.env.VITE_MINIMUM_PLASTIK_TO_VOTE;
  console.log("minimumPlastikToVote", minimumPlastikToVote);

  // Calculate vote percentages
  const totalVotes = votesFor + votesAgainst;
  const votesForPercentage = totalVotes > 0 ? (votesFor / totalVotes) * 100 : 0;
  const votesAgainstPercentage =
    totalVotes > 0 ? (votesAgainst / totalVotes) * 100 : 0;

  // Reset selection when hasVoted changes
  useEffect(() => {
    if (hasVoted) {
      setSelectedOption(null);
    }
  }, [hasVoted]);

  const castVote = async () => {
    if (!selectedOption || hasVoted || !eligibleforVoting) return;

    const inFavor = selectedOption === "new";
    await onVote(inFavor);
  };

  // Determine if button should be disabled
  const isButtonDisabled =
    !eligibleforVoting || !selectedOption || hasVoted || isVoting;

  return (
    <div className="bg-white p-4 sm:p-6 rounded-2xl shadow w-full">
      <div className="flex flex-col sm:flex-row sm:justify-between items-start sm:items-center mb-4">
        <h2 className="text-2xl font-semibold">Token Retirement Rate Vote</h2>
        {countdown && (
          <div className="mt-2 sm:mt-0 bg-blue-50 px-3 py-1 rounded-lg">
            <p className="text-sm font-medium text-blue-700">
              Time remaining: {countdown}
            </p>
          </div>
        )}
      </div>

      <p className="mb-4 text-sm text-gray-500">
        Cast your vote to determine the percentage of tokens to be retired from
        DEX transactions.
      </p>

      {/* User has already voted message */}
      {hasVoted && (
        <div className="mb-4 bg-green-50 border border-green-200 p-4 rounded-lg">
          <p className="text-green-800 font-medium text-sm">
            ✓ You have already voted on this proposal
          </p>
        </div>
      )}

      {/* Keep Current Rate Option (Vote Against) */}
      <div
        className={`mb-4 border p-4 sm:p-5 rounded-lg font-bold transition-all ${
          selectedOption === "old"
            ? "border-[#082FB9] bg-blue-50"
            : "border-gray-200"
        } ${hasVoted ? "opacity-60 cursor-not-allowed" : "cursor-pointer"}`}
        onClick={() => {
          if (!hasVoted && eligibleforVoting) {
            setSelectedOption("old");
          }
        }}
      >
        <label className="flex items-center mb-2">
          <input
            type="radio"
            name="vote"
            className="mr-2 w-5 h-5 appearance-none border border-gray-500 rounded-full checked:bg-[#082FB9] checked:border-black relative before:content-[''] before:absolute before:inset-[-4px] before:border-2 before:border-gray-500 before:rounded-full before:scale-0 checked:before:scale-100 transition-all disabled:cursor-not-allowed disabled:opacity-50"
            checked={selectedOption === "old"}
            disabled={hasVoted || !eligibleforVoting}
            onChange={() => {
              if (!hasVoted && eligibleforVoting) {
                setSelectedOption("old");
              }
            }}
          />
          <span className={hasVoted ? "text-gray-500" : ""}>
            Maintain {oldRetirementRate}% Token Retirement
          </span>
        </label>
        <p className="text-right text-sm sm:text-base mb-1">
          {votesAgainst} vote{votesAgainst !== 1 ? "s" : ""} (
          {votesAgainstPercentage.toFixed(1)}%)
        </p>
        <div className="bg-gray-300 h-2 rounded-full overflow-hidden">
          <div
            className="bg-red-500 h-2 rounded-full transition-all duration-500"
            style={{ width: `${votesAgainstPercentage}%` }}
          />
        </div>
      </div>

      {/* New Rate Option (Vote For) */}
      <div
        className={`mb-4 border p-4 sm:p-5 rounded-lg font-bold transition-all ${
          selectedOption === "new"
            ? "border-[#082FB9] bg-blue-50"
            : "border-gray-200"
        } ${hasVoted ? "opacity-60 cursor-not-allowed" : "cursor-pointer"}`}
        onClick={() => {
          if (!hasVoted && eligibleforVoting) {
            setSelectedOption("new");
          }
        }}
      >
        <label className="flex items-center mb-2">
          <input
            type="radio"
            name="vote"
            className="mr-2 w-5 h-5 appearance-none border rounded-full checked:bg-[#082FB9] checked:border-none relative before:content-[''] before:absolute before:inset-[-4px] before:border-2 before:border-gray-500 before:rounded-full before:scale-0 checked:before:scale-100 transition-all disabled:cursor-not-allowed disabled:opacity-50"
            checked={selectedOption === "new"}
            disabled={hasVoted || !eligibleforVoting}
            onChange={() => {
              if (!hasVoted && eligibleforVoting) {
                setSelectedOption("new");
              }
            }}
          />
          <span className={hasVoted ? "text-gray-500" : ""}>
            {newRetirementRate > oldRetirementRate ? "Increase" : "Change"} to{" "}
            {newRetirementRate}% Token Retirement
          </span>
        </label>
        <p className="text-right text-sm sm:text-base mb-1">
          {votesFor} vote{votesFor !== 1 ? "s" : ""} (
          {votesForPercentage.toFixed(1)}%)
        </p>
        <div className="bg-gray-300 h-2 rounded-full overflow-hidden">
          <div
            className="bg-[#082FB9] h-2 rounded-full transition-all duration-500"
            style={{ width: `${votesForPercentage}%` }}
          />
        </div>
      </div>

      {/* Vote Summary */}
      <div className="mb-4 bg-gray-50 p-3 rounded-lg">
        <div className="flex justify-between text-sm">
          <span className="text-gray-600">Total Votes:</span>
          <span className="font-semibold">{totalVotes}</span>
        </div>
        {totalVotes > 0 && (
          <div className="flex justify-between text-sm mt-1">
            <span className="text-gray-600">Current Leader:</span>
            <span className="font-semibold text-[#082FB9]">
              {votesFor > votesAgainst
                ? `New Rate (${newRetirementRate}%)`
                : votesFor < votesAgainst
                ? `Current Rate (${oldRetirementRate}%)`
                : "Tied"}
            </span>
          </div>
        )}
      </div>

      {/* Eligibility Messages */}
      {!eligibleforVoting && (
        <div className="mb-4 bg-yellow-50 border border-yellow-200 p-3 rounded-lg">
          <p className="text-sm text-yellow-800">
            ⚠️ You must hold at least {minimumPlastikToVote} PLASTIK tokens to
            be eligible to vote.
          </p>
        </div>
      )}

      {eligibleforVoting && !hasVoted && !selectedOption && (
        <p className="text-sm text-gray-500 mb-4">
          *Select an option above to cast your vote
        </p>
      )}

      {/* Cast Vote Button */}
      <div className="p-4 rounded-lg flex justify-center">
        <Button
          variant="userButton"
          className={`w-full sm:w-1/3 font-semibold rounded-4xl text-white transition-all ${
            isButtonDisabled
              ? "bg-gray-400 cursor-not-allowed"
              : "bg-[#082FB9] hover:bg-blue-700"
          }`}
          onClick={castVote}
          disabled={isButtonDisabled}
        >
          {isVoting
            ? "Submitting Vote..."
            : hasVoted
            ? "Vote Submitted"
            : "Cast Your Vote"}
        </Button>
      </div>

      {/* Additional Info */}
      {selectedOption && !hasVoted && eligibleforVoting && (
        <div className="mt-4 p-3 bg-blue-50 rounded-lg">
          <p className="text-sm text-blue-800">
            {selectedOption === "new" ? (
              <>
                You are voting <strong>FOR</strong> changing the retirement rate
                to {newRetirementRate}%
              </>
            ) : (
              <>
                You are voting <strong>AGAINST</strong> the proposed change and
                want to keep the rate at {oldRetirementRate}%
              </>
            )}
          </p>
        </div>
      )}
    </div>
  );
};

export default Voting;
