import DynamicTable from "../../components/Table";
import StatsCard from "../../components/StatsCard";
// import { faFire, faLeaf } from "@fortawesome/free-solid-svg-icons";
import { Recycle, CoinsSwap, DownArrow } from "@/assets/icons";
import { Loader } from "lucide-react";
import { useSelector } from "react-redux";
import { RootState } from "@/redux/store";
import { useMemo, useState } from "react";
import CheckboxFilterList, {
  FilterOption,
} from "@/components/CheckboxFilterList";

const transactionsFilterOptions: FilterOption[] = [
  { id: "creditSale", label: "Plastic Credit Sale" },
  { id: "roadmapCompletion", label: "Roadmap Completion" },
  { id: "tokenReturn", label: "Token Return" },
  { id: "usdmReleased", label: "USDM Released" },
];

const Transactions = ({
  transactions,
  loading,
  error,
  page,
  count,
  onPageChange,
}: any) => {
  const [filterType, setFilterType] = useState<string>(
    transactionsFilterOptions[0].id
  );
  const [showFilters, setShowFilters] = useState(false);

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

  const getTransactions = () => {
    switch (filterType) {
      case "creditSale":
        return transactions.filter(
          (tx: { type: string }) => tx.type === "creditSale"
        );
      case "roadmapCompletion":
        return transactions.filter(
          (tx: { type: string }) => tx.type === "roadmapCompletion"
        );
      case "tokenReturn":
        return transactions.filter(
          (tx: { type: string }) => tx.type === "tokenReturn"
        );
      case "usdmReleased":
        return transactions.filter(
          (tx: { type: string }) => tx.type === "usdmReleased"
        );
      default:
        return transactions;
    }
  };

  const displayedTransactions = getTransactions();

  const getFilterValue = () => {
    switch (filterType) {
      case "creditSale":
        return "Plastic Credit";
      case "roadmapCompletion":
        return "Plastik";
      case "tokenReturn":
        return "Plastik";
      case "usdmReleased":
        return "USDM";
      default:
        return "";
    }
  };

  const toggleFilters = () => setShowFilters((prev) => !prev);

  // handle single-select change
  const handleFilterChange = (selected: string[]) => {
    if (selected.length) {
      setFilterType(selected[0]);
      setShowFilters(false);
    }
  };

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
      <div className="flex justify-between items-center mb-6">
        <div>
          <h2 className="text-2xl font-semibold text-[#0D0D0D]">
            {transactionsFilterOptions.find((f) => f.id === filterType)?.label}{" "}
            Transactions
          </h2>
          <p className="text-[#0D0D0D] mb-4">
            This table displays all verified{" "}
            {transactionsFilterOptions.find((f) => f.id === filterType)?.label}{" "}
            processed via the ReFi DApp.
          </p>
        </div>
        <div className="relative inline-block">
          <button
            onClick={toggleFilters}
            className="rounded-full border border-[#D4D9D8] px-3 py-1.5 flex items-center gap-2"
          >
            <p>Filter By</p>
            <img src={DownArrow} alt="DownArrow" />
          </button>
          {showFilters && (
            <div className="absolute right-0 mt-2 w-60 max-w-md sm:w-60 rounded-2xl shadow-lg z-10 border bg-white border-[#D4D9D8]">
              <CheckboxFilterList
                options={transactionsFilterOptions}
                initialSelected={[filterType]}
                onChange={handleFilterChange}
              />
            </div>
          )}
        </div>
      </div>

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
          <DynamicTable
            transactions={displayedTransactions}
            filterValue={getFilterValue()}
          />

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
