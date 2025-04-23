import { useState } from "react";
import RoadmapCard from "./RoadmapCard";
import { faFilter } from "@fortawesome/free-solid-svg-icons";
import Button from "./Button";
import { useSelector } from "react-redux";
import { RootState } from "../redux/store";
import { Loader2, X } from "lucide-react";

const Roadmaps = () => {
  const [filterType, setFilterType] = useState<"active" | "completed">(
    "active"
  );
  const [searchTerm, setSearchTerm] = useState("");
  const [showFilters, setShowFilters] = useState(false);
  const [dropdownOpen, setDropdownOpen] = useState(false);

  const { roadmaps, loading, error } = useSelector(
    (state: RootState) => state.roadmaps
  );

  const {
    roadmaps: completedRoadmaps,
    loading: completedRoadmapsLoading,
    error: completedRoadmapsError,
  } = useSelector((state: RootState) => state.completedRoadmaps);

  const toggleFilters = () => setShowFilters((prev) => !prev);

  const isLoading =
    filterType === "active" ? loading : completedRoadmapsLoading;
  const isError = filterType === "active" ? error : completedRoadmapsError;
  const allData = filterType === "active" ? roadmaps : completedRoadmaps;

  const filteredData = allData.filter((r) =>
    r.roadmapName.toLowerCase().includes(searchTerm.toLowerCase())
  );

  return (
    <div className="bg-white text-black mx-auto px-4 md:px-10 lg:px-20 py-6">
      <div className="flex justify-between items-center mb-6">
        <h2 className="text-2xl font-bold">
          {filterType === "active" ? "Active Roadmaps" : "Completed Roadmaps"}
        </h2>
        <div className="relative inline-block">
          <Button variant="dark" icon={faFilter} onClick={toggleFilters}>
            Filters
          </Button>

          {showFilters && (
            <div className="absolute right-0 mt-2 w-80 max-w-xs sm:w-80 bg-gray-100 p-4 rounded-md shadow-lg z-10">
              <button
                className="absolute top-2 right-2 w-6 h-6 flex items-center justify-center bg-red-500 text-white rounded-full focus:outline-none cursor-pointer"
                onClick={toggleFilters}
              >
                <X className="w-4 h-4" />
              </button>

              <div className="mb-3 pt-2">
                <label className="block mb-1 font-medium text-sm">
                  Search Roadmaps
                </label>
                <input
                  type="text"
                  className="w-full border border-gray-300 rounded-md px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-blue-500"
                  placeholder="Search by name..."
                  value={searchTerm}
                  onChange={(e) => setSearchTerm(e.target.value)}
                />
              </div>

              <div className="flex flex-col gap-2 text-sm relative">
                <label className="font-medium">Show:</label>
                <div className="relative">
                  <div
                    onClick={() => setDropdownOpen(!dropdownOpen)}
                    className="mt-1 w-full appearance-none border border-gray-300 rounded-md px-3 py-2 pr-8 focus:outline-none focus:ring-2 focus:ring-blue-500 bg-white text-black cursor-pointer"
                  >
                    {filterType === "active"
                      ? "Active Roadmaps"
                      : "Completed Roadmaps"}
                  </div>

                  {/* Arrow Icon */}
                  <div className="pointer-events-none absolute top-1/2 right-3 transform -translate-y-1/2 text-gray-500">
                    ▼
                  </div>

                  {/* Dropdown Options */}
                  {dropdownOpen && (
                    <div className="absolute w-full mt-1 bg-white border border-gray-300 rounded-md shadow-lg z-10">
                      <div
                        onClick={() => {
                          setFilterType("active");
                          setDropdownOpen(false);
                        }}
                        className={`p-2 cursor-pointer hover:bg-gray-200 ${
                          filterType === "active"
                            ? "text-blue-500 font-semibold"
                            : ""
                        }`}
                      >
                        Active Roadmaps
                      </div>
                      <div
                        onClick={() => {
                          setFilterType("completed");
                          setDropdownOpen(false);
                        }}
                        className={`p-2 cursor-pointer hover:bg-gray-200 ${
                          filterType === "completed"
                            ? "text-blue-500 font-semibold"
                            : ""
                        }`}
                      >
                        Completed Roadmaps
                      </div>
                    </div>
                  )}
                </div>
              </div>
            </div>
          )}
        </div>
      </div>

      {filterType === "active" ? (
        <p className="text-gray-600 mb-6">
          Active roadmaps are currently in progress and are being actively
          managed. These roadmaps represent ongoing efforts to recover plastic
          waste and promote sustainability.
        </p>
      ) : (
        <p className="text-gray-600 mb-6">
          Completed roadmaps showcase the successful completion of various
          plastic recovery projects. These roadmaps highlight the achievements
          and milestones reached in the journey toward sustainability and
          reducing plastic waste.
        </p>
      )}

      {isLoading ? (
        <div className="flex justify-center items-center h-32">
          <Loader2 className="animate-spin w-10 h-10 text-gray-600" />
        </div>
      ) : isError ? (
        <p className="text-red-600">Error: {isError}</p>
      ) : filteredData.length === 0 ? (
        <p className="text-gray-600">No matching roadmaps found.</p>
      ) : (
        filteredData.map((roadmap, index) => (
          <RoadmapCard key={index} {...roadmap} />
        ))
      )}
    </div>
  );
};

export default Roadmaps;
