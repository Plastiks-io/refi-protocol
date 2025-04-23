import { useState } from "react";
import { faCircleCheck } from "@fortawesome/free-solid-svg-icons";
import Button from "../Button";

interface VotingRetirementProps {
  eligibleforVoting: boolean;
  oldRetirementRate: number;
  newRetirementRate: number;
  oldRetirementPercentage: number;
  newRetirementPercentage: number;
}

const StartVote = ({
  eligibleforVoting,
  oldRetirementRate,
  newRetirementRate,
  oldRetirementPercentage,
  newRetirementPercentage,
}: VotingRetirementProps) => {
  // State to manage selected vote option
  const [selectedOption, setSelectedOption] = useState<string | null>(null);
  const [selectedVoteRate, setSelectedVoteRate] = useState<Number | 0>(0);

  const castVote = async () => {
    alert(selectedVoteRate);
  };

  return (
    <div className="bg-white p-4 rounded-lg border border-gray-200">
      <div className="flex justify-between">
        <h2 className="text-2xl font-semibold">Token Retirement Rate Vote</h2>
        <Button
          icon={eligibleforVoting ? faCircleCheck : undefined}
          variant="userButton"
          className="bg-gray-900 text-white rounded-full"
        >
          {eligibleforVoting ? "Eligible to Vote" : "Not Eligible to Vote"}
        </Button>
      </div>
      <p className="mb-4 text-sm text-gray-500">
        Cast your vote to determine the percentage of tokens to be retired from
        DEX transactions.
      </p>

      {/* First Vote Option */}
      <div
        className="mb-4 border border-gray-200 p-5 rounded-lg font-bold cursor-pointer"
        onClick={() => {
          setSelectedOption("old");
          setSelectedVoteRate(oldRetirementRate);
        }}
      >
        <label className="flex items-center mb-2">
          <input
            type="radio"
            name="vote"
            className="mr-2 w-5 h-5 appearance-none border border-gray-500 rounded-full checked:bg-black checked:border-black relative before:content-[''] before:absolute before:inset-[-4px] before:border-2 before:border-gray-500 before:rounded-full before:scale-0 checked:before:scale-100 transition-all"
            checked={selectedOption === "old"}
            onChange={() => {
              setSelectedOption("old");
              setSelectedVoteRate(oldRetirementRate);
            }}
          />
          Maintain {oldRetirementRate}% Token Retirement
        </label>
        <p className="text-right">89,425 votes</p>
        <div className="bg-gray-300 h-2 rounded-full mb-2">
          <div
            className="bg-black h-2 rounded-full"
            style={{ width: `${oldRetirementPercentage}%` }}
          ></div>
        </div>
      </div>

      {/* Second Vote Option */}
      <div
        className="mb-4 border border-gray-200 p-5 rounded-lg font-bold cursor-pointer"
        onClick={() => {
          setSelectedOption("new");
          setSelectedVoteRate(newRetirementRate);
        }}
      >
        <label className="flex items-center mb-2">
          <input
            type="radio"
            name="vote"
            className="mr-2 w-5 h-5 appearance-none border border-gray-500 rounded-full checked:bg-black checked:border-black relative before:content-[''] before:absolute before:inset-[-4px] before:border-2 before:border-gray-500 before:rounded-full before:scale-0 checked:before:scale-100 transition-all"
            checked={selectedOption === "new"}
            onChange={() => {
              setSelectedOption("new");
              setSelectedVoteRate(newRetirementRate);
            }}
          />
          Increase to {newRetirementRate}% Token Retirement
        </label>
        <p className="text-right">67,317 votes</p>
        <div className="bg-gray-300 h-2 rounded-full mb-2">
          <div
            className="bg-black h-2 rounded-full"
            style={{ width: `${newRetirementPercentage}%` }}
          ></div>
        </div>
      </div>

      {/* Cast Vote Button */}
      <Button variant="dark" className="w-full" onClick={castVote}>
        Cast Your Vote
      </Button>
      <p className="text-sm text-gray-500 mt-2 text-center">
        You must hold at least 1000 PLASTIK tokens to be eligible to vote.
      </p>
    </div>
  );
};

export default StartVote;
