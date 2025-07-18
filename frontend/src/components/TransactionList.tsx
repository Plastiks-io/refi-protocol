import { Transaction, TransactionType } from "@/redux/TransactionSlice";
import { formatDateTime } from "@/utils/helper";
import { ArrowRight, Recycle } from "lucide-react";

interface TransactionListProps {
  transactions: Transaction[];
  roadmapId?: string;
}

const TransactionList: React.FC<TransactionListProps> = ({
  transactions,
  roadmapId,
}) => {
  const filteredTransactions = roadmapId
    ? transactions.filter((tx) => tx.roadmapId === roadmapId)
    : transactions;

  return (
    <div className="mt4">
      <h2 className="text-xl font-semibold mb-2">Transaction History</h2>
      <p className="text-sm text-gray-500 mb-4">
        Below is a record of recent transactions, providing details about key
        activities and their timestamps for better tracking and transparency.
      </p>
      <div className="bg-white rounded-xl shadow-md overflow-hidden border border-[#E5E7EB]">
        {filteredTransactions.map((tx, idx) => {
          // only render Sold or Transfer types
          if (
            tx.type === TransactionType.Sold ||
            tx.type === TransactionType.Transfer
          ) {
            const { datePart, timePart } = formatDateTime(tx.txDate);
            const isLast = idx === filteredTransactions.length - 1;

            return (
              <div
                key={tx.id}
                className={`flex items-center justify-between px-4 py-4 sm:px-6 ${
                  !isLast ? "border-b border-gray-200" : ""
                }`}
              >
                {/* Left section: icon + title/subtitle */}
                <div className="flex items-center">
                  <div className="flex-shrink-0 bg-[#082FB9] text-white rounded-full p-2">
                    {tx.type === TransactionType.Transfer ? (
                      <ArrowRight />
                    ) : (
                      <Recycle />
                    )}
                  </div>
                  <div className="ml-4">
                    <p className="text-base font-medium text-gray-900">
                      {tx.type
                        .replace(/([A-Z])/g, " $1")
                        .replace(/^./, (str) => str.toUpperCase())}
                    </p>
                    <p className="mt-1 text-sm text-gray-600">
                      {tx.type === TransactionType.Transfer
                        ? `${tx.amount} USDM to Escrow`
                        : `${tx.amount} kg of plastic`}
                    </p>
                  </div>
                </div>

                {/* Right section: date/time */}
                <div className="flex flex-col items-end">
                  <p className="text-sm font-medium text-gray-900">
                    {datePart}
                  </p>
                  <p className="mt-1 text-xs text-gray-500">{timePart}</p>
                </div>
              </div>
            );
          }
          return null;
        })}
      </div>
    </div>
  );
};

export default TransactionList;
