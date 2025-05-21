import { useState } from "react";
import RoadmapCard from "./RoadmapCard";
import CheckboxFilterList, { FilterOption } from "./CheckboxFilterList";
import { DownArrow } from "@/assets/icons";
import { useSelector } from "react-redux";
import { RootState } from "../redux/store";
import { Loader2, X } from "lucide-react";

const filterOptions: FilterOption[] = [
  { id: "active", label: "In Progress" },
  { id: "completed", label: "Complete" },
  { id: "missingFunds", label: "Missing Funds" },
  { id: "newest", label: "Newest" },
  { id: "oldest", label: "Oldest" },
  { id: "moreKgs", label: "More Kgs" },
  { id: "lessKgs", label: "Less Kgs" },
];

const Roadmaps = () => {
  const [filterType, setFilterType] = useState<string>(filterOptions[0].id);
  const [showFilters, setShowFilters] = useState(false);

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

  const toggleFilters = () => setShowFilters((prev) => !prev);

  // handle single-select change
  const handleFilterChange = (selected: string[]) => {
    if (selected.length) {
      setFilterType(selected[0]);
      setShowFilters(false);
    }
  };

  // compute loading and error
  const isLoading =
    filterType === "active"
      ? loadingActive
      : filterType === "completed"
      ? loadingCompleted
      : false;

  const hasError =
    (filterType === "active" && !!errorActive) ||
    (filterType === "completed" && !!errorCompleted) ||
    // for global sorts, check all
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
        // combine all
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

  return (
    <div className="bg-white text-black mx-auto px-4 md:px-10 lg:px-20 py-6">
      <div className="flex justify-between items-center mb-6">
        <h2 className="text-2xl font-bold">
          {filterOptions.find((f) => f.id === filterType)?.label} Roadmaps
        </h2>
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

      <p className="text-gray-600 mb-6">
        Active roadmaps are currently in progress and are being actively
        managed.
      </p>

      {isLoading ? (
        <div className="flex justify-center items-center h-32" role="status">
          <Loader2 className="animate-spin w-10 h-10 text-gray-600" />
        </div>
      ) : hasError ? (
        <div className="flex items-center gap-2 text-red-600">
          <X className="w-5 h-5" />
          <span>Failed to load roadmaps. Please try again later.</span>
        </div>
      ) : (
        <div>
          {displayedRoadmaps.map((r) => (
            <RoadmapCard key={r.roadmapId} {...r} />
          ))}
        </div>
      )}
    </div>
  );
};

export default Roadmaps;
