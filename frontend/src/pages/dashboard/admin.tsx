import { useSelector } from "react-redux";
import RoadmapRow from "../../components/RoadmapRow";
import { RootState } from "../../redux/store";
import { AlertCircle, Loader } from "lucide-react";

const admin = () => {
  const { roadmaps, loading, error } = useSelector(
    (state: RootState) => state.roadmaps
  );
  return (
    <div className="mx-auto px-4 py-6 bg-white text-black md:px-10 lg:px-20 min-h-screen">
      <div className="mb-8">
        <h2 className="text-2xl font-bold mb-2">Active Roadmaps</h2>
        <p className="text-gray-600 mb-6">
          Below is a list of active roadmaps, their progress, and associated
          details to help you manage ongoing initiatives effectively.
        </p>

        {loading ? (
          <div className="flex items-center justify-center py-10">
            <Loader className="animate-spin text-blue-400" size={32} />
            <span className="ml-2 text-gray-500">Loading roadmaps...</span>
          </div>
        ) : error ? (
          <div className="flex items-center justify-center py-10 text-red-500">
            <AlertCircle size={32} />
            <span className="ml-2">
              Failed to load roadmaps. Please try again.
            </span>
          </div>
        ) : roadmaps.length === 0 ? (
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
                  <th className="p-4 font-medium">Actions</th>
                </tr>
              </thead>
              <tbody>
                {roadmaps.map((data, index) => (
                  <RoadmapRow key={index} {...data} />
                ))}
              </tbody>
            </table>
          </div>
        )}
      </div>

      <div>
        <h2 className="text-xl font-semibold mb-2">Transaction History</h2>
        <p className="text-sm text-gray-500 mb-4">
          Below is a record of recent transactions, providing details about key
          activities and their timestamps for better tracking and transparency.
        </p>
        {/* <div className="border rounded-md shadow divide-y divide-gray-200">
          {transactions.map((item, idx) => (
            <TransactionItem key={idx} {...item} />
          ))}
        </div> */}
      </div>
    </div>
  );
};

export default admin;
