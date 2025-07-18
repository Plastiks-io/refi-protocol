import { useCallback, useMemo, useRef } from "react";
import { useSelector } from "react-redux";
import StatsCard from "./StatsCard";
import { Loader2, X } from "lucide-react";
import {
  Flag,
  CircleCheckBig,
  Recycle,
  Wallet,
  CoinsHand,
  CoinsSwap,
} from "@/assets/icons";
import { RootState } from "@/redux/store";

const HeroSection = () => {
  // selectors for active, completed, archived
  const {
    roadmaps: activeRoadmaps,
    loading: loadingActive,
    error: errorActive,
  } = useSelector((state: RootState) => state.roadmaps);
  const {
    roadmaps: completedRoadmaps,
    loading: loadingCompleted,
    error: errorCompleted,
  } = useSelector((state: RootState) => state.completedRoadmaps);

  // compute dynamic stats
  const activeCount = activeRoadmaps.length;
  const totalCompletedCount = completedRoadmaps.length;
  const combinedRoadmaps = useMemo(
    () => [...activeRoadmaps, ...completedRoadmaps],
    [activeRoadmaps, completedRoadmaps]
  );
  const totalRecoveredKg = useMemo(
    () =>
      combinedRoadmaps.reduce((sum, r) => sum + (r.recoveredPlastic || 0), 0),
    [combinedRoadmaps]
  );

  // placeholder for funds and tokens until real data available
  // replace these with selectors or API data when ready
  const fundsInEscrow = useMemo(
    () =>
      activeRoadmaps.reduce((sum, r) => sum + (r.soldPlasticCredits || 0), 0),
    [activeRoadmaps]
  );
  const totalFundingDistributed = useMemo(
    () =>
      completedRoadmaps.reduce(
        (sum, r) => sum + (r.soldPlasticCredits || 0),
        0
      ),
    [completedRoadmaps]
  );
  // plastik token retired = sold p.c * 100 * 2%
  const totalTokensRetired = useMemo(
    () =>
      completedRoadmaps.reduce(
        (sum, r) => sum + (r.soldPlasticCredits || 0) * 2,
        0
      ),
    [completedRoadmaps]
  );

  // 1. Create a ref on the element at the bottom of the page
  const bottomRef = useRef<HTMLDivElement>(null);

  // 2. Extract a scroll handler
  const scrollToBottom = useCallback(() => {
    if (bottomRef.current) {
      bottomRef.current.scrollIntoView({ behavior: "smooth" });
    }
  }, []);

  const stats = [
    {
      title: "Active Roadmaps",
      value: activeCount.toString(),
      description: "Currently in progress from verified recovery entities",
      icon: Flag,
    },
    {
      title: "Total Completed Roadmaps",
      value: totalCompletedCount.toString(),
      description: "Fully financed and finalized roadmaps",
      icon: CircleCheckBig,
    },
    {
      title: "Plastic Recovered",
      value: `${totalRecoveredKg.toLocaleString()} kg`,
      description: "Verified, recovered, and recorded on the blockchain",
      icon: Recycle,
    },
    {
      title: "Funds in Escrow Wallet",
      value: fundsInEscrow.toString(),
      description: "USDM",
      icon: Wallet,
    },
    {
      title: "Total Funding Distributed",
      value: totalFundingDistributed.toString(),
      description: "USDM",
      icon: CoinsHand,
    },
    {
      title: "Total Tokens Retired",
      value: `${Math.round(totalTokensRetired).toLocaleString()}`,
      description: "PLASTIK",
      icon: CoinsSwap,
    },
  ];

  const isLoading = loadingActive || loadingCompleted;
  const hasError = !!errorActive || !!errorCompleted;

  return (
    <div>
      <div className="bg-[linear-gradient(to_right,_#082FB9_0%,_#0D0D0D_25%,_#0D0D0D_75%,_#082FB9_100%)] md:min-h-[500px] flex justify-center items-center">
        {/* Hero Section */}
        <section className="text-white text-center py-12 px-4">
          <div className="max-w-2xl mx-auto">
            <h1 className="text-3xl sm:text-4xl font-bold">
              Building a Sustainable Future with ReFi
            </h1>
            <p className="mt-4 text-base sm:text-lg">
              Plastiks ReFi Protocol ensures transparent and traceable funding
              for plastic recovery initiatives, empowering communities to combat
              plastic pollution.
            </p>
          </div>
          <button
            className="mt-6 px-8 sm:px-10 py-2 bg-[#082FB9] text-white font-semibold rounded-full text-sm sm:text-base cursor-pointer"
            onClick={scrollToBottom}
          >
            Roadmaps
          </button>
        </section>
      </div>

      {/* Stats Section */}
      <div className="bg-white">
        <section className="relative flex flex-wrap justify-center gap-6 py-8 px-4">
          {isLoading ? (
            <div className="flex justify-center items-center w-full py-10">
              <Loader2 className="animate-spin w-10 h-10 text-gray-600" />
            </div>
          ) : hasError ? (
            <div className="flex items-center gap-2 text-red-600 w-full py-10 justify-center">
              <X className="w-5 h-5" />
              <span>Failed to load stats. Please try again later.</span>
            </div>
          ) : (
            stats.map((stat, index) => (
              <StatsCard
                key={index}
                title={stat.title}
                value={stat.value}
                description={stat.description}
                icon={stat.icon}
                width="1/4"
              />
            ))
          )}
        </section>
      </div>
      {/* 3. Place this div at the very bottom */}
      <div ref={bottomRef} />
    </div>
  );
};

export default HeroSection;
