import { CheckCircle, XCircle2 } from "@/assets/icons";

interface TokenBalanceProps {
  plastikBalance: number;
  votingAllowed: boolean;
  voteStarted?: boolean;
}

const TokenBalance = ({
  plastikBalance,
  votingAllowed,
  voteStarted,
}: TokenBalanceProps) => {
  return (
    <div className="bg-gray-100 p-4 rounded-lg mb-6 border border-gray-200 w-full">
      <div className="flex flex-col sm:flex-row sm:items-center sm:justify-between gap-4">
        {/* Token Info */}
        <div>
          <h2 className="text-lg font-semibold">Your Token Balance</h2>
          <p className="text-2xl font-bold">
            {new Intl.NumberFormat("en-US").format(plastikBalance)} Plastik
          </p>
        </div>

        {/* Button */}
        <div
          className={`px-4 py-2 flex items-center justify-center gap-2 ${
            votingAllowed
              ? "bg-[#A7FE8A] text-black  font-md rounded-4xl"
              : voteStarted
              ? "bg-[#85D7FE] text-black  font-md rounded-4xl"
              : "bg-[#FEDC85] text-black  font-md rounded-4xl"
          }`}
        >
          <img
            src={
              votingAllowed ? CheckCircle : voteStarted ? CheckCircle : XCircle2
            }
            alt={
              votingAllowed
                ? "Check Icon"
                : voteStarted
                ? "Vote Icon"
                : "Close Icon"
            }
            className="h-5 w-5"
          />
          {votingAllowed
            ? "Eligible to Start Voting Round"
            : voteStarted
            ? "Voting Round is Active, You Can Vote"
            : "Not Eligible to Start Voting Round"}
        </div>
      </div>
    </div>
  );
};

export default TokenBalance;
