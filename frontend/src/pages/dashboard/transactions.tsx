import DynamicTable from "../../components/DynamicTable";
import StatsCard from "../../components/StatsCard";
// import { faFire, faLeaf } from "@fortawesome/free-solid-svg-icons";
import { Recycle, CoinsSwap, DownArrow } from "@/assets/icons";
import { Loader } from "lucide-react";
import { useDispatch, useSelector } from "react-redux";
import { AppDispatch, RootState } from "@/redux/store";
import { useEffect, useMemo, useState } from "react";
import CheckboxFilterList, {
  FilterOption,
} from "@/components/CheckboxFilterList";
import ReactPaginate from "react-paginate";
import { fetchTransactions, TransactionType } from "@/redux/TransactionSlice";

const transactionsFilterOptions: FilterOption[] = [
  { id: TransactionType.Sold, label: "Plastic Credit Sale" },
  { id: TransactionType.Roadmap, label: "Roadmap Completion" },
  { id: TransactionType.Token, label: "Token Return" },
  { id: TransactionType.USDM, label: "USDM Released" },
];

const Transactions = ({
  transactions,
  loading,
  error,
  page,
  totalPages,
  onPageChange,
}: any) => {
  const [filterType, setFilterType] = useState<TransactionType[]>([
    transactionsFilterOptions[0].id as TransactionType,
  ]);

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

  const getFilterValue = () => {
    switch (
      filterType[0] // assuming filterType is an array with a single value
    ) {
      case TransactionType.Sold:
        return "Plastic Credit";
      case TransactionType.Roadmap:
        return "Plastik";
      case TransactionType.Token:
        return "Plastik";
      case TransactionType.USDM:
        return "USDM";
      default:
        return "Token";
    }
  };

  const toggleFilters = () => setShowFilters((prev) => !prev);

  // handle single-select change
  const handleFilterChange = (selected: string[]) => {
    if (selected.length) {
      setFilterType([selected[0] as TransactionType]);
      setShowFilters(false);
      // 👇 Reset to page 1 after filter change
      onPageChange(1, selected[0] as any);
    }
  };

  const dispatch = useDispatch<AppDispatch>();
  useEffect(() => {
    dispatch(fetchTransactions({ page: 1, perPage: 10, type: filterType }));
  }, [filterType]);

  // When clicking pagination buttons
  const handlePageChangeInternal = (selectedPage: number) => {
    onPageChange(selectedPage, filterType[0] as TransactionType);
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
            {
              transactionsFilterOptions.find((f) => f.id === filterType[0])
                ?.label
            }{" "}
            Transactions
          </h2>
          <p className="text-[#0D0D0D] mb-4">
            This table displays all verified{" "}
            {
              transactionsFilterOptions.find((f) => f.id === filterType[0])
                ?.label
            }{" "}
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
                initialSelected={filterType}
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
            transactions={transactions}
            filterValue={getFilterValue()}
          />
          <ReactPaginate
            forcePage={page - 1}
            pageCount={totalPages}
            onPageChange={({ selected }) =>
              handlePageChangeInternal(selected + 1)
            }
            // show first 2 pages and last 2 pages only
            marginPagesDisplayed={2}
            pageRangeDisplayed={0}
            breakLabel="…"
            previousLabel="Prev"
            nextLabel="Next"
            containerClassName="flex justify-center items-center space-x-2 mt-6"
            pageClassName="px-3 py-1 border border-gray-300 rounded hover:bg-gray-100"
            pageLinkClassName="text-gray-700"
            activeClassName="bg-blue-500"
            activeLinkClassName="text-white"
            previousClassName="px-3 py-1 border border-gray-300 rounded-l hover:bg-gray-100"
            nextClassName="px-3 py-1 border border-gray-300 rounded-r hover:bg-gray-100"
            previousLinkClassName="text-gray-700"
            nextLinkClassName="text-gray-700"
            disabledClassName="opacity-50 cursor-not-allowed"
            breakClassName="px-2 text-gray-500"
            breakLinkClassName="text-gray-500"
          />
        </>
      )}
    </div>
  );
};

export default Transactions;
