import React from "react";
import { Transaction } from "../redux/TransactionSlice";

interface Props {
  transactions: Transaction[];
}

const DynamicTable: React.FC<Props> = ({ transactions }) => {
  if (!transactions || transactions.length === 0) {
    return (
      <p className="text-red-600 text-center mt-4 font-semibold text-xl">
        No transactions available please connect your wallet.
      </p>
    );
  }

  // Define fixed column widths (optional: adjust per column)
  const columnWidths: Record<string, string> = {
    date: "w-[80px]",
    transactionFee: "w-[80px]",
    amount: "w-[150px]",
    tokenId: "w-[150px]",
    direction: "w-[80px]",
    pcAssetId: "w-[80px]",
    hash: "w-[80px]",
  };

  return (
    <div className="overflow-x-auto rounded-lg">
      <table className="min-w-full bg-white border border-gray-200 shadow-md">
        <thead>
          <tr className="bg-gray-100">
            {Object.keys(transactions[0]).map((key) => (
              <th
                key={key}
                className={`px-4 py-3 text-left font-semibold text-black capitalize ${
                  columnWidths[key] || "w-[120px]"
                }`}
              >
                {key
                  .replace(/([A-Z])/g, " $1")
                  .replace(/^./, (str) => str.toUpperCase())}
              </th>
            ))}
          </tr>
        </thead>
        <tbody>
          {transactions.map((tx, index) => (
            <tr key={index} className="border-t border-gray-200">
              {Object.entries(tx).map(([key, value], i) => {
                // Conditional text color for direction
                const isDirection = key === "direction";
                const directionClass =
                  value === "sent"
                    ? "text-red-600 font-semibold"
                    : value === "received"
                    ? "text-green-600 font-semibold"
                    : "";

                return (
                  <td
                    key={i}
                    className={`px-4 py-3 break-words whitespace-normal align-top ${
                      columnWidths[key] || "w-[120px]"
                    } ${isDirection ? directionClass : "text-gray-700"}`}
                  >
                    {typeof value === "string" && value.startsWith("http") ? (
                      <a
                        href={value}
                        className="text-blue-600 underline break-all"
                        target="_blank"
                      >
                        Link
                      </a>
                    ) : (
                      <p className="break-all">{value}</p>
                    )}
                  </td>
                );
              })}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
};

export default DynamicTable;
