// src/pages/dashboard/admin.tsx
import { useDispatch, useSelector } from "react-redux";
import RoadmapRow from "../../components/RoadmapRow";
import { AppDispatch, RootState } from "../../redux/store";
import { AlertCircle, Loader } from "lucide-react";
import { useEffect, useState } from "react";
import { DownArrow } from "@/assets/icons";
import CheckboxFilterList, {
  FilterOption,
} from "@/components/CheckboxFilterList";
import { fetchTransactions, TransactionType } from "@/redux/TransactionSlice";
import TransactionList from "@/components/TransactionList";
import ReactPaginate from "react-paginate";

const filterOptions: FilterOption[] = [
  { id: "active", label: "In Progress" },
  { id: "completed", label: "Complete" },
  { id: "missingFunds", label: "Missing Funds" },
  { id: "newest", label: "Newest" },
  { id: "oldest", label: "Oldest" },
  { id: "moreKgs", label: "More Kgs" },
  { id: "lessKgs", label: "Less Kgs" },
];

const AdminPage: React.FC = () => {
  // selectors for active, completed
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

  const { transactions, page, perPage, totalPages } = useSelector(
    (state: RootState) => state.transactions
  );

  const [showFilters, setShowFilters] = useState(false);
  const toggleFilters = () => setShowFilters((prev) => !prev);
  const [filterType, setFilterType] = useState<string>(filterOptions[0].id);

  const handleFilterChange = (selected: string[]) => {
    if (selected.length) {
      setFilterType(selected[0]);
      setShowFilters(false);
    }
  };

  const isLoading =
    filterType === "active"
      ? loadingActive
      : filterType === "completed"
      ? loadingCompleted
      : false;

  const hasError =
    (filterType === "active" && !!errorActive) ||
    (filterType === "completed" && !!errorCompleted) ||
    (["newest", "oldest", "moreKgs", "lessKgs"].includes(filterType) &&
      (!!errorActive || !!errorCompleted));

  const getDisplayed = () => {
    switch (filterType) {
      case "active":
      case "missingFunds":
        return activeRoadmaps;
      case "completed":
        return completedRoadmaps;
      case "newest":
        return [...activeRoadmaps, ...completedRoadmaps].sort(
          (a, b) =>
            new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime()
        );
      case "oldest":
        return [...activeRoadmaps, ...completedRoadmaps].sort(
          (a, b) =>
            new Date(a.createdAt).getTime() - new Date(b.createdAt).getTime()
        );
      case "moreKgs":
        return [...activeRoadmaps, ...completedRoadmaps].sort(
          (a, b) => b.totalPlastic - a.totalPlastic
        );
      case "lessKgs":
        return [...activeRoadmaps, ...completedRoadmaps].sort(
          (a, b) => a.totalPlastic - b.totalPlastic
        );
      default:
        return activeRoadmaps;
    }
  };

  const displayedRoadmaps = getDisplayed();

  const handlePageChange = (selected: number) => {
    dispatch(
      fetchTransactions({
        page: selected,
        perPage: 10,
        type: [TransactionType.Sold, TransactionType.Transfer],
      })
    );
  };
  const dispatch = useDispatch<AppDispatch>();

  useEffect(() => {
    // for admin, fetch CreditSale + FundTransfer
    dispatch(
      fetchTransactions({
        page: 1,
        perPage,
        type: [TransactionType.Sold, TransactionType.Transfer],
      })
    );
  }, [dispatch]);
  // Otherwise, show the table & filters
  return (
    <div className="mx-auto px-4 py-6 bg-white text-black md:px-10 lg:px-20 min-h-screen">
      <div className="mb-8">
        <h2 className="text-2xl font-bold mb-2">Active Roadmaps</h2>
        <div className="flex items-center justify-between mb-6">
          <p className="text-gray-600 text-wrap">
            Below is a list of active roadmaps, their progress, and associated
            details to help you manage ongoing initiatives effectively.
          </p>
          <div className="relative inline-block">
            <button
              onClick={toggleFilters}
              className="rounded-full border border-[#D4D9D8] px-3 py-1.5 flex items-center gap-2"
            >
              <p>Filter By</p>
              <img src={DownArrow} alt="DownArrow" />
            </button>
            {showFilters && (
              <div className="absolute right-0 mt-2 w-50 max-w-xs sm:w-50 rounded-2xl shadow-lg z-10 border bg-white border-[#D4D9D8]">
                <CheckboxFilterList
                  options={filterOptions}
                  initialSelected={[filterType]}
                  onChange={handleFilterChange}
                />
              </div>
            )}
          </div>
        </div>

        {isLoading ? (
          <div className="flex items-center justify-center py-10">
            <Loader className="animate-spin text-blue-400" size={32} />
            <span className="ml-2 text-gray-500">Loading roadmaps...</span>
          </div>
        ) : hasError ? (
          <div className="flex items-center justify-center py-10 text-red-500">
            <AlertCircle size={32} />
            <span className="ml-2">
              Failed to load roadmaps. Please try again.
            </span>
          </div>
        ) : displayedRoadmaps.length === 0 ? (
          <div className="flex items-center justify-center py-10 text-gray-500">
            <AlertCircle size={32} />
            <span className="ml-2">No roadmaps available.</span>
          </div>
        ) : (
          <div className="overflow-auto rounded-md shadow">
            <table className="min-w-full text-left">
              <thead className="bg-gray-100">
                <tr>
                  <th className="p-4 font-medium">Entity</th>
                  <th className="p-4 font-medium">Roadmap</th>
                  <th className="p-4 font-medium">Date</th>
                  <th className="p-4 font-medium">Progress</th>
                  <th className="p-4 font-medium">Total Escrow</th>
                  <th className="p-4 font-medium">Total Tokens</th>
                  <th className="p-4 font-medium">Fund Status</th>
                  <th className="p-4 font-medium">View Details</th>
                </tr>
              </thead>
              <tbody>
                {displayedRoadmaps.map((data, index) => (
                  <RoadmapRow key={index} {...data} />
                ))}
              </tbody>
            </table>
          </div>
        )}
      </div>
      {/* Transaction History of fund transfered */}
      <div className="w-full mx-auto mt-5">
        <TransactionList transactions={transactions} />
        <ReactPaginate
          forcePage={page - 1}
          pageCount={totalPages}
          onPageChange={({ selected }) => handlePageChange(selected + 1)}
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
      </div>
    </div>
  );
};

export default AdminPage;
