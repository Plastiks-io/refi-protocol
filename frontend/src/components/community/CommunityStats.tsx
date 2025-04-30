type Stats = {
  currentRetirementRate: string;
  totalVotes: string;
  totalPlastikVoted: string;
  remainingTime: string;
};

type CommunityStatsProps = {
  stats: Stats;
  votingAllowed: boolean;
};

const CommunityStats = ({ stats, votingAllowed }: CommunityStatsProps) => {
  return (
    <div
      className={`bg-white p-4 rounded-lg mb-6  ${
        !votingAllowed ? "border border-gray-200" : ""
      }`}
    >
      <h2 className="font-bold text-xl">Current Statistics</h2>
      <div
        className={`${
          votingAllowed
            ? "flex flex-col gap-4 mt-4"
            : "flex justify-between flex-wrap gap-4 mt-4"
        }`}
      >
        {Object.entries(stats).map(([key, value]) => (
          <div key={key}>
            <h1 className="text-gray-500">
              {key
                .replace(/([A-Z])/g, " $1")
                .replace(/^./, (str) => str.toUpperCase())}{" "}
            </h1>
            <p className="text-2xl font-bold">{value}</p>
          </div>
        ))}
      </div>
    </div>
  );
};

export default CommunityStats;
