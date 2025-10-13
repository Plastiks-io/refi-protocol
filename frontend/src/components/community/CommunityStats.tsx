import { useEffect, useState } from "react";

export type Stats = {
  currentRetirementRate: string;
  roundDuration: string; // may hold a date range or end date
  totalVotes: string;
  totalPlastikVoted: string;
};

type CommunityStatsProps = {
  stats: Stats;
  voting?: boolean; // pass true if voting is ongoing
  votingEndDate?: Date; // required if voting is true
};

const CommunityStats = ({
  stats,
  voting,
  votingEndDate,
}: CommunityStatsProps) => {
  const [countdown, setCountdown] = useState<string>("");

  // Helper to format countdown string
  const getCountdownString = (end: Date) => {
    const now = new Date();
    const diff = end.getTime() - now.getTime();
    if (diff <= 0) return "0d 0h 0m";
    const days = Math.floor(diff / (1000 * 60 * 60 * 24));
    const hours = Math.floor((diff / (1000 * 60 * 60)) % 24);
    const minutes = Math.floor((diff / (1000 * 60)) % 60);
    return `${days}d ${hours}h ${minutes}m`;
  };

  // Update countdown every minute if voting is ongoing
  useEffect(() => {
    if (!voting || !votingEndDate) return;
    setCountdown(getCountdownString(votingEndDate));
    const interval = setInterval(() => {
      setCountdown(getCountdownString(votingEndDate));
    }, 60000);
    return () => clearInterval(interval);
  }, [voting, votingEndDate]);

  return (
    <div className="flex flex-wrap justify-between gap-4 mb-6">
      {/* Current Retirement Rate */}
      <div className="w-full sm:w-[48%] lg:w-[24%] bg-white p-6 rounded-2xl shadow border border-gray-200">
        <h1 className="text-black text-lg font-semibold mb-1">
          Current Retirement Rate
        </h1>
        <p className="text-2xl font-bold text-black">
          {stats.currentRetirementRate}
        </p>
      </div>

      {/* Duration / Voting Timer */}
      <div className="w-full sm:w-[48%] lg:w-[24%] bg-white p-6 rounded-2xl shadow border border-gray-200">
        <h1 className="text-black text-lg font-semibold mb-1">
          {voting ? "Voting Ends In" : "Next Voting Round"}
        </h1>
        <p className="text-2xl font-bold text-black">
          {voting && votingEndDate ? countdown : stats.roundDuration}
        </p>
      </div>

      {/* Total Votes */}
      <div className="w-full sm:w-[48%] lg:w-[24%] bg-white p-6 rounded-2xl shadow border border-gray-200">
        <h1 className="text-black text-lg font-semibold mb-1">Total Votes</h1>
        <p className="text-2xl font-bold text-black">{stats.totalVotes}</p>
      </div>

      {/* Total PLASTIK Voted */}
      <div className="w-full sm:w-[48%] lg:w-[24%] bg-white p-6 rounded-2xl shadow border border-gray-200">
        <h1 className="text-black text-lg font-semibold mb-1">
          Total Plastiks Voted
        </h1>
        <p className="text-2xl font-bold text-black">
          {stats.totalPlastikVoted}
        </p>
      </div>
    </div>
  );
};

export default CommunityStats;
