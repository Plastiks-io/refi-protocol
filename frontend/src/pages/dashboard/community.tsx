import { useContext, useEffect, useState } from "react";
import Button from "../../components/Button";
import CommunityStats from "../../components/community/CommunityStats";
import TokenBalance from "../../components/community/TokenBalance";
import StartVote from "../../components/community/StartVote";
import VotingPopup from "../../components/community/VotingPopup";
import { toast } from "sonner";
import { WalletContext } from "../../App";

const stats = {
  currentRetirementRate: "2%",
  totalVotes: "15,672",
  totalPlastikVoted: "45.2M",
  remainingTime: "2d 6h 32m",
};

const percentages = [2, 3, 4, 5];
const Community: React.FC = () => {

  const [plastikBalance, setPlastikBalance] = useState<number>(0);
  const [votingAllowed, setVotingAllowed] = useState<boolean>(false);
  const [eligibleforVoting, setEligibleforVoting] = useState<boolean>(false);
  const [voting, setVoting] = useState<boolean>(false);
  const [votingPopup, setVotingPopup] = useState<boolean>(false);
  const [votingPercentage, setVotingPercentage] = useState<number>(0);

  // Data from Environment Variables
  const plastikTokenAddress = import.meta.env.VITE_PLASTIK_TOKEN_ADDRESS;
  const minimumPlastikToInitializeVoting = import.meta.env.VITE_MINIMUM_PLASTIK;
  const minimumPlastikToVote = import.meta.env.VITE_MINIMUM_PLASTIK_TO_VOTE;

  const wallet = useContext(WalletContext);
  const fetchBalances = async () => {
    if (!wallet) return;
    const balances = await wallet.getBalance();
    console.log(balances);
    console.log("plastikTokenAddress", plastikTokenAddress);

    const plastik = balances.find(
      (item) => item.unit === plastikTokenAddress
    )?.quantity;
    console.log(plastik);
    setPlastikBalance(plastik ? Number(plastik) : 0);
    // Check if user is eligible to vote
    setVotingAllowed(
      plastik ? Number(plastik) >= minimumPlastikToInitializeVoting : false
    );
    setEligibleforVoting(
      plastik ? Number(plastik) >= minimumPlastikToVote : false
    );
  };

  const startVoting = () => {
    toast.success("Success message!");
    setVotingPopup(false);
    setVoting(true);
    console.log("Voting started");
  };

  useEffect(() => {
    if (wallet === null) {
      setPlastikBalance(0);
      setVotingAllowed(false);
      setEligibleforVoting(false);
      setVoting(false);
      setVotingPercentage(0);
      return;
    }
    fetchBalances();
  }, [wallet]);

  const votingDuration = 30;
  const votingDate = new Date();
  votingDate.setDate(votingDate.getDate() + votingDuration);

  const votingEndDate = votingDate.toLocaleDateString("en-US", {
    day: "numeric",
    month: "long",
    year: "numeric",
  });

  return (
    <div className="bg-gray-50 text-black mx-auto px-4 md:px-10 lg:px-20 py-6 min-h-[calc(80vh)]">
      <h1 className="text-2xl font-bold mb-2">Community Voting</h1>
      <p className="mb-6 text-sm text-gray-600">
        Create or participate in votes to determine token retirement percentage.
        Participate in shaping the future of Plastiks ecosystem.
      </p>
      {votingAllowed && (
        <div className="bg-gray-200 p-4 rounded-lg mb-6">
          <h2 className="text-black font-medium">Minimum requirements</h2>
          <div className="ml-5">
            <p className="text-gray-600 text-sm">
              Holds atleast {minimumPlastikToInitializeVoting} plastik Tokens
            </p>
            <p className="text-gray-600 text-sm">
              {votingDuration}-day voting period
            </p>
            <p className="text-gray-600 text-sm">Clear proposal description</p>
          </div>
        </div>
      )}

      {votingAllowed ? (
        <div className="flex gap-6">
          <div className="bg-white p-4 rounded-lg border border-gray-200 mb-6 w-2/3">
            <h1 className="text-center text-2xl font-semibold mb-4">
              Start New Voting Round
            </h1>
            {/* Token Balance */}
            <TokenBalance
              plastikBalance={plastikBalance}
              votingAllowed={votingAllowed}
            />
            <p>Select Retirement Percentage</p>
            <div className="flex items-center gap-4 mt-4">
              {percentages.map((percentage) => (
                <Button
                  key={percentage}
                  variant="userButton"
                  className={`w-1/4 text-2xl border border-gray-300 rounded-lg font-semibold cursor-pointer ${
                    votingPercentage == percentage
                      ? "bg-black text-gray-100"
                      : "text-black"
                  }`}
                  onClick={() => setVotingPercentage(percentage)}
                >
                  {percentage}%
                </Button>
              ))}
            </div>
            <div className="bg-gray-200 p-4 rounded-lg mt-4">
              <h2 className="text-black text-xl font-medium">
                Voting Duration
              </h2>
              <p className="text-gray-600">
                The voting round will last for {votingDuration} days until{" "}
                {votingEndDate}
              </p>
            </div>
            <Button
              variant="dark"
              className="w-full mt-4"
              onClick={() => setVotingPopup(true)}
            >
              Start Voting Round
            </Button>
          </div>
          <div className="w-1/3 bg-white p-4 rounded-lg border border-gray-200 mb-6">
            <CommunityStats stats={stats} votingAllowed={votingAllowed} />
          </div>
        </div>
      ) : (
        <div>
          {/* Current Statistics */}
          <CommunityStats stats={stats} votingAllowed={votingAllowed} />
          <TokenBalance
            plastikBalance={plastikBalance}
            votingAllowed={votingAllowed}
          />
        </div>
      )}

      {/* Token Retirement Rate Vote */}
      {voting && (
        <StartVote
          eligibleforVoting={eligibleforVoting}
          oldRetirementRate={2}
          newRetirementRate={votingPercentage}
          oldRetirementPercentage={20}
          newRetirementPercentage={50}
        />
      )}

      {votingPopup && (
        <VotingPopup
          votingPercentage={votingPercentage}
          votingDuration={votingDuration}
          onStartVoting={startVoting}
          onClose={() => setVotingPopup(false)}
        />
      )}
    </div>
  );
};

export default Community;
