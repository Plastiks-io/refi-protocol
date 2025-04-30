import DynamicTable from "../../components/Table";
import StatsCard from "../../components/StatsCard";
import { faFire, faLeaf } from "@fortawesome/free-solid-svg-icons";
import { Loader } from "lucide-react";

const stats = [
  {
    title: "Total Tokens Retired",
    value: "234,567",
    description: "+2.5% from last month",
    icon: faFire,
  },
  {
    title: "Plastic Recovered",
    value: "45,678 kg",
    description: "Equivalent to x amount of bottles",
    icon: faLeaf,
  },
];

const Transactions = ({
  transactions,
  loading,
  error,
  page,
  count,
  onPageChange,
}: any) => {
  return (
    <div className="bg-white text-gray-500 mx-auto px-4 md:px-10 lg:px-20 py-6 min-h-[calc(80vh)]">
      <section className="relative flex flex-wrap justify-center gap-6 py-8 px-4">
        {stats.map((stat, index) => (
          <StatsCard
            key={index}
            title={stat.title}
            value={stat.value}
            description={stat.description}
            icon={stat.icon}
            width="2/5"
          />
        ))}
      </section>

      <h2 className="text-xl font-semibold text-gray-700">Transactions</h2>
      <p className="text-gray-500 mb-4">
        Below is a detailed list of transactions history showcasing the fees,
        amounts, and associated plastic credits. This data highlights the impact
        and transparency of our plastic recovery initiatives.
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
