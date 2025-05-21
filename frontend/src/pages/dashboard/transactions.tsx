import DynamicTable from "../../components/Table";
import StatsCard from "../../components/StatsCard";
// import { faFire, faLeaf } from "@fortawesome/free-solid-svg-icons";
import { Recycle, CoinsSwap } from "@/assets/icons";
import { Loader } from "lucide-react";
import { useSelector } from "react-redux";
import { RootState } from "@/redux/store";
import { useMemo } from "react";

const Transactions = ({
  transactions,
  loading,
  error,
  page,
  count,
  onPageChange,
}: any) => {
  // selectors for active, completed, archived
  const { roadmaps: activeRoadmaps } = useSelector(
    (state: RootState) => state.roadmaps
  );
  const { roadmaps: completedRoadmaps } = useSelector(
    (state: RootState) => state.completedRoadmaps
  );
  const { roadmaps: archivedRoadmaps } = useSelector(
    (state: RootState) => state.archivedRoadmaps
  );

  const combinedRoadmaps = useMemo(
    () => [...activeRoadmaps, ...completedRoadmaps, ...archivedRoadmaps],
    [activeRoadmaps, completedRoadmaps, archivedRoadmaps]
  );
  const totalRecoveredKg = useMemo(
    () =>
      combinedRoadmaps.reduce((sum, r) => sum + (r.recoveredPlastic || 0), 0),
    [combinedRoadmaps]
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

  const stats = [
    {
      title: "Total Tokens Retired",
      value: totalTokensRetired.toString(),
      description: "Plastik",
      icon: CoinsSwap,
    },
    // 1 bottle = 18 gm so 1 kg = 56 bottles
    {
      title: "Plastic Recovered",
      value: `${totalRecoveredKg.toLocaleString()} kg`,
      description: `Equivalent to ${totalRecoveredKg * 56} amount of bottles`,
      icon: Recycle,
    },
  ];

  return (
    <div className="bg-white text-gray-500 mx-auto px-4 md:px-10 lg:px-20 py-6 min-h-[calc(80vh)]">
      <section className="relative flex flex-wrap gap-2 justify-around py-8 px-4 mb-4">
        {stats.map((stat, index) => (
          <StatsCard
            key={index}
            title={stat.title}
            value={stat.value}
            description={stat.description}
            icon={stat.icon}
            width="12/25"
          />
        ))}
      </section>

      <h2 className="text-2xl font-semibold text-[#0D0D0D]">
        Plastic Credit Transactions
      </h2>
      <p className="text-[#0D0D0D] mb-4">
        This table displays all verified plastic credit sales processed via the
        ReFi DApp.
      </p>

      {loading ? (
        <div className="flex justify-center items-center py-10">
          <Loader className="animate-spin w-8 h-8 text-blue-500" />
          <span className="ml-3 text-blue-500">Loading transactions...</span>
        </div>
      ) : error ? (
        <div className="flex justify-center items-center py-10 text-red-500">
          {typeof error === "string" ? error : "Failed to load transactions."}
        </div>
      ) : (
        <>
          <DynamicTable transactions={transactions} />

          <div className="flex justify-center mt-6 gap-0.5">
            <button
              onClick={() => onPageChange(page - 1)} // Call the callback on page change
              disabled={page <= 1}
              className="px-4 py-2 bg-blue-500 text-white rounded-l-lg disabled:bg-gray-300"
            >
              Previous
            </button>
            <button
              onClick={() => onPageChange(page + 1)} // Call the callback on page change
              disabled={transactions.length < count}
              className="px-4 py-2 bg-blue-500 text-white rounded-r-lg disabled:bg-gray-300"
            >
              Next
            </button>
          </div>
        </>
      )}
    </div>
  );
};

export default Transactions;
