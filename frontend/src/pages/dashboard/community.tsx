import { useContext, useEffect, useState } from "react";
import { useSelector, useDispatch } from "react-redux";
import Button from "../../components/Button";
import CommunityStats from "../../components/community/CommunityStats";
import TokenBalance from "../../components/community/TokenBalance";
import VotingPopup from "../../components/community/VotingPopup";
import Voting from "@/components/community/Voting";
import { toast } from "sonner";
import { WalletContext } from "../../App";
import { fetchGovernanceStats } from "../../redux/governanceSlice";
import type { RootState, AppDispatch } from "../../redux/store";
import { governanceClient } from "@/services/governance";

const percentages = [2, 3, 4, 5];

export interface ActiveProposal {
  proposalId: string;
  retirementPercentage: string;
  proposerPkh: string;
  createdAt: string;
  expiresAt: string;
  votesFor: string;
  votesAgainst: string;
  executed: boolean;
}

const Community: React.FC = () => {
  const [plastikBalance, setPlastikBalance] = useState<number>(0);
  const [votingAllowed, setVotingAllowed] = useState<boolean>(false);
  const [eligibleforVoting, setEligibleforVoting] = useState<boolean>(false);
  const [votingPercentage, setVotingPercentage] = useState<number>(0);
  const [activeProposal, setActiveProposal] = useState<ActiveProposal | null>(
    null
  );
  const [isLoadingProposal, setIsLoadingProposal] = useState<boolean>(false);
  const [votingPopup, setVotingPopup] = useState<boolean>(false);
  const [isCreatingProposal, setIsCreatingProposal] = useState<boolean>(false);
  const [isVoting, setIsVoting] = useState<boolean>(false);
  const [hasVoted, setHasVoted] = useState<boolean>(false);
  const [canExecute, setCanExecute] = useState<boolean>(false);
  const [isExecuting, setIsExecuting] = useState<boolean>(false);
  const [countdown, setCountdown] = useState<string>("");

  // Derived state
  const isVotingActive = activeProposal !== null;
  const dispatch = useDispatch<AppDispatch>();
  const {
    data: governanceData,
    loading: governanceLoading,
    error: governanceError,
  } = useSelector((state: RootState) => state.governance);

  const policyId = import.meta.env.VITE_POLICY_ID;
  const plastikTokenName = import.meta.env.VITE_PLASTIC_TOKEN_NAME;
  const plastikTokenAddress = `${policyId}${plastikTokenName}`;
  const minimumPlastikToInitializeVoting = Number(
    import.meta.env.VITE_MINIMUM_PLASTIK
  );
  const minimumPlastikToVote = Number(
    import.meta.env.VITE_MINIMUM_PLASTIK_TO_VOTE
  );
  const votingDuration = 30;
  const wallet = useContext(WalletContext);

  // Format countdown
  const getCountdownString = (end: Date) => {
    const now = new Date();
    const diff = end.getTime() - now.getTime();
    if (diff <= 0) return "0d 0h 0m";
    const days = Math.floor(diff / (1000 * 60 * 60 * 24));
    const hours = Math.floor((diff / (1000 * 60 * 60)) % 24);
    const minutes = Math.floor((diff / (1000 * 60)) % 60);
    return `${days}d ${hours}h ${minutes}m`;
  };

  // Fetch token balances
  const fetchBalances = async () => {
    if (!wallet) return;
    try {
      const balances = await wallet.getBalance();
      const plastik = balances.find(
        (item) => item.unit === plastikTokenAddress
      )?.quantity;
      const parsed = plastik ? Number(plastik) : 0;

      setPlastikBalance(parsed);
      setVotingAllowed(parsed >= minimumPlastikToInitializeVoting);
      setEligibleforVoting(parsed >= minimumPlastikToVote);
    } catch (err) {
      console.error("Failed to fetch balances:", err);
    }
  };

  // Check if user has voted
  const checkUserVoted = async () => {
    if (!wallet || !activeProposal) {
      setHasVoted(false);
      return;
    }
    try {
      const voted = await governanceClient.hasUserVoted(
        wallet,
        BigInt(activeProposal.proposalId)
      );
      setHasVoted(voted);
    } catch (err) {
      console.error("Failed to check vote status:", err);
      setHasVoted(false);
    }
  };

  // Create a new proposal
  const startVoting = async () => {
    if (!wallet || votingPercentage === 0) {
      toast.error("Please select a retirement percentage");
      return;
    }

    setIsCreatingProposal(true);
    try {
      const txHash = await governanceClient.createProposal(
        wallet,
        BigInt(votingPercentage)
      );

      toast.success(`Proposal created successfully! Transaction: ${txHash}`, {
        closeButton: true,
        duration: 5000,
      });
      setVotingPopup(false);
      setVotingPercentage(0);
    } catch (error) {
      console.error("Error creating proposal:", error);
      toast.error(
        error instanceof Error ? error.message : "Failed to create proposal",
        { duration: 5000 }
      );
    } finally {
      setIsCreatingProposal(false);
    }
  };

  // Vote on active proposal
  const voteOnProposal = async (inFavor: boolean) => {
    if (!wallet || !activeProposal) return;

    setIsVoting(true);
    try {
      const txHash = await governanceClient.voteOnProposal(
        inFavor,
        wallet,
        BigInt(activeProposal.proposalId)
      );

      toast.success(
        `Vote ${
          inFavor ? "for" : "against"
        } submitted successfully! Tx: ${txHash}`,
        {
          closeButton: true,
          duration: 5000,
        }
      );
    } catch (error) {
      console.error("Error voting:", error);
      toast.error(
        error instanceof Error ? error.message : "Failed to submit vote",
        { duration: 5000 }
      );
    } finally {
      setIsVoting(false);
    }
  };

  // Execute and finalize proposal
  const executeProposal = async () => {
    if (!wallet || !activeProposal) return;

    setIsExecuting(true);
    try {
      const txHash = await governanceClient.executeAndUpdateProposal(
        wallet,
        BigInt(activeProposal.proposalId)
      );

      toast.success(`Proposal executed and finalized! Transaction: ${txHash}`, {
        closeButton: true,
        duration: 5000,
      });
    } catch (error) {
      console.error("Error executing proposal:", error);
      toast.error(
        error instanceof Error ? error.message : "Failed to execute proposal",
        { duration: 5000 }
      );
    } finally {
      setIsExecuting(false);
    }
  };

  // Update countdown every minute when there's an active proposal
  useEffect(() => {
    if (!activeProposal) {
      setCountdown("");
      return;
    }

    const updateCountdown = () => {
      const expiryDate = new Date(Number(activeProposal.expiresAt));
      setCountdown(getCountdownString(expiryDate));
    };

    updateCountdown();
    const interval = setInterval(updateCountdown, 60000);
    return () => clearInterval(interval);
  }, [activeProposal]);

  // Check if proposal can be executed
  useEffect(() => {
    if (!activeProposal) {
      setCanExecute(false);
      return;
    }

    const now = Date.now();
    const expired = now > Number(activeProposal.expiresAt);
    const notExecuted = !activeProposal.executed;

    setCanExecute(expired && notExecuted);
  }, [activeProposal]);

  // Update local activeProposal when governance data changes
  useEffect(() => {
    setIsLoadingProposal(governanceLoading);

    if (!governanceData) {
      setActiveProposal(null);
      return;
    }

    if (governanceData.hasActiveProposal && governanceData.activeProposal) {
      try {
        const ap = governanceData.activeProposal;
        setActiveProposal(ap as ActiveProposal);
      } catch (err) {
        console.error(
          "Failed to parse active proposal from governanceData:",
          err
        );
        setActiveProposal(null);
      }
    } else {
      setActiveProposal(null);
    }
  }, [governanceData, governanceLoading]);

  // Check vote status when proposal or wallet changes
  useEffect(() => {
    if (activeProposal && wallet) {
      checkUserVoted();
    }
  }, [activeProposal, wallet]);

  // Fetch token balances when wallet changes
  useEffect(() => {
    if (wallet === null) {
      setPlastikBalance(0);
      setVotingAllowed(false);
      setEligibleforVoting(false);
      setActiveProposal(null);
      setVotingPercentage(0);
      setHasVoted(false);
      return;
    }
    fetchBalances();
  }, [wallet]);

  // Fetch governance stats on component mount and when wallet changes
  useEffect(() => {
    dispatch(fetchGovernanceStats());
  }, [dispatch, wallet]);

  // Loader component
  const GovernanceStatsLoader = () => (
    <div className="w-full rounded-lg mb-6">
      <div className="flex flex-wrap justify-between gap-4 mb-6">
        {[1, 2, 3, 4].map((index) => (
          <div
            key={index}
            className="w-full sm:w-[48%] lg:w-[24%] bg-white p-6 rounded-2xl shadow border border-gray-200 flex flex-col justify-center items-start animate-pulse"
          >
            <div className="h-5 bg-gray-300 rounded w-3/4 mb-2"></div>
            <div className="h-8 bg-gray-300 rounded w-1/2"></div>
          </div>
        ))}
      </div>
    </div>
  );

  // Error component
  const GovernanceStatsError = ({ error }: { error: string }) => (
    <div className="w-full rounded-lg mb-6">
      <div className="bg-red-50 border border-red-200 rounded-lg p-4">
        <div className="flex items-center">
          <div className="text-red-600">
            <p className="font-medium">Error loading governance stats</p>
            <p className="text-sm mt-1">{error}</p>
          </div>
          <button
            onClick={() => dispatch(fetchGovernanceStats())}
            className="ml-auto px-3 py-1 text-sm bg-red-600 text-white rounded hover:bg-red-700"
          >
            Retry
          </button>
        </div>
      </div>
    </div>
  );

  // Precompute formatted voting end date for create panel UI
  const votingEndDate = new Date();
  votingEndDate.setDate(votingEndDate.getDate() + votingDuration);
  const votingEndDateFormatted = votingEndDate.toLocaleDateString("en-US", {
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

      <div className="flex flex-col gap-3">
        {/* Governance Stats - Always visible regardless of wallet connection */}
        {governanceLoading ? (
          <GovernanceStatsLoader />
        ) : governanceError ? (
          <GovernanceStatsError error={governanceError} />
        ) : governanceData ? (
          <div className="w-full rounded-lg mb-6">
            <CommunityStats
              stats={{
                currentRetirementRate: governanceData.currentRetirementRate,
                roundDuration: governanceData.roundDuration,
                totalVotes: governanceData.totalVotes,
                totalPlastikVoted: governanceData.totalPlastikVoted,
              }}
              voting={isVotingActive}
              votingEndDate={
                activeProposal
                  ? new Date(Number(activeProposal.expiresAt))
                  : undefined
              }
            />
          </div>
        ) : (
          <div className="w-full rounded-lg mb-6">
            <div className="bg-gray-100 border border-gray-200 rounded-lg p-6 text-center">
              <p className="text-gray-600">No governance stats available</p>
              <button
                onClick={() => dispatch(fetchGovernanceStats())}
                className="mt-2 px-4 py-2 text-sm bg-blue-600 text-white rounded hover:bg-blue-700"
              >
                Load Stats
              </button>
            </div>
          </div>
        )}

        <div className="bg-[#F1F4FE] p-4 rounded-lg mb-6">
          <h2 className="text-[#0D0D0D] font-semibold">Minimum requirements</h2>
          <div className="ml-5 flex gap-2 flex-col">
            <p className="text-gray-600">
              You can participate by voting for your preferred token retirement
              rate below.
            </p>
            <p className="text-gray-600">
              You must hold at least {minimumPlastikToInitializeVoting} PLASTIK
              tokens to start a voting round.
            </p>
            <p className="text-gray-600">
              You must hold at least {minimumPlastikToVote} PLASTIK tokens to
              vote.
            </p>
            <p className="text-gray-600">{votingDuration}-day voting period.</p>
          </div>
        </div>

        {/* Token Balance - Only show when wallet connected */}
        {wallet && (
          <TokenBalance
            plastikBalance={plastikBalance}
            votingAllowed={votingAllowed}
          />
        )}

        {/* Active Proposal Voting Section */}
        {wallet && isVotingActive && !governanceLoading && !canExecute && (
          <Voting
            eligibleforVoting={eligibleforVoting}
            oldRetirementRate={Number(
              governanceData?.currentRetirementRate.split("%")[0] || "0"
            )}
            newRetirementRate={Number(
              activeProposal?.retirementPercentage || "0"
            )}
            oldRetirementPercentage={
              activeProposal
                ? Number(
                    governanceData?.currentRetirementRate.split("%")[0] || "0"
                  )
                : 0
            }
            newRetirementPercentage={
              activeProposal ? Number(activeProposal.retirementPercentage) : 0
            }
            hasVoted={hasVoted}
            onVote={voteOnProposal}
            isVoting={isVoting}
            votesFor={Number(activeProposal?.votesFor || "0")}
            votesAgainst={Number(activeProposal?.votesAgainst || "0")}
            countdown={countdown}
          />
        )}

        {/* Expired Proposal Section - Can be executed by anyone */}
        {wallet && canExecute && (
          <div className="bg-orange-50 p-4 rounded-lg border border-orange-200 mb-6 w-full">
            <h3 className="text-lg font-semibold text-orange-800 mb-2">
              Proposal Voting Ended
            </h3>
            <p className="text-orange-700 mb-2">
              This proposal has expired and can now be executed by anyone.
            </p>
            <p className="text-orange-600 text-sm mb-4">
              Execution will automatically apply the new retirement rate if the
              proposal passed (quorum reached and more votes for than against).
            </p>

            {activeProposal && (
              <div className="bg-white p-3 rounded-lg mb-4 text-sm">
                <p className="text-gray-700">
                  <span className="font-semibold">Votes For:</span>{" "}
                  {activeProposal.votesFor}
                </p>
                <p className="text-gray-700">
                  <span className="font-semibold">Votes Against:</span>{" "}
                  {activeProposal.votesAgainst}
                </p>
                <p className="text-gray-700">
                  <span className="font-semibold">Proposed Rate:</span>{" "}
                  {activeProposal.retirementPercentage}%
                </p>
              </div>
            )}

            <Button
              variant="userButton"
              className="w-full font-semibold rounded-lg text-white bg-orange-600 hover:bg-orange-700 disabled:opacity-50 disabled:cursor-not-allowed"
              onClick={executeProposal}
              disabled={isExecuting}
            >
              {isExecuting ? "Executing..." : "Execute & Finalize Proposal"}
            </Button>
          </div>
        )}

        {/* Create New Proposal Section - Only show if no active proposal */}
        {wallet && !activeProposal && !governanceLoading && (
          <div className="bg-white p-4 rounded-lg border border-gray-200 mb-6 w-full">
            <p className="text-base sm:text-lg font-medium">
              Select Retirement Percentage
            </p>

            <div className="flex flex-col sm:flex-row items-center justify-evenly mt-4 gap-2">
              {percentages.map((percentage) => (
                <Button
                  key={percentage}
                  variant="userButton"
                  className={`w-full text-xl sm:text-2xl border border-gray-300 rounded-lg font-semibold cursor-pointer text-center ${
                    votingPercentage === percentage
                      ? "bg-black text-white"
                      : "text-black hover:bg-gray-100"
                  }`}
                  onClick={() => setVotingPercentage(percentage)}
                >
                  {percentage}%
                </Button>
              ))}
            </div>

            <div className="bg-[#F4F4F4] p-4 rounded-lg mt-4">
              <h2 className="text-black text-lg sm:text-xl font-medium">
                Voting Duration
              </h2>
              <p className="text-[#525252] text-sm sm:text-base">
                The voting round will last for {votingDuration} days until{" "}
                {votingEndDateFormatted}
              </p>
            </div>

            <div className="flex justify-center mt-4">
              <Button
                variant="userButton"
                className="w-full sm:w-[40%] font-semibold rounded-4xl text-white bg-[#082FB9] disabled:opacity-50 disabled:cursor-not-allowed"
                onClick={() => setVotingPopup(true)}
                disabled={
                  !votingAllowed || isLoadingProposal || votingPercentage === 0
                }
              >
                {isLoadingProposal ? "Loading..." : "Start Voting Round"}
              </Button>
            </div>
          </div>
        )}

        {/* Loading state for proposal operations */}
        {isLoadingProposal && (
          <div className="bg-blue-50 p-4 rounded-lg border border-blue-200 mb-6 w-full text-center">
            <p className="text-blue-700 font-medium">
              Loading governance state...
            </p>
          </div>
        )}

        {/* Wallet not connected message */}
        {!wallet && (
          <div className="bg-yellow-50 p-4 rounded-lg border border-yellow-200 mb-6 w-full text-center">
            <p className="text-yellow-800 font-medium">
              Please connect your wallet to participate in governance
            </p>
          </div>
        )}
      </div>

      {/* Voting Popup */}
      {votingPopup && (
        <VotingPopup
          votingPercentage={votingPercentage}
          votingDuration={votingDuration}
          onStartVoting={startVoting}
          onClose={() => setVotingPopup(false)}
          isCreating={isCreatingProposal}
        />
      )}
    </div>
  );
};

export default Community;
