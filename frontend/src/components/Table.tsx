import { Transaction } from "@/redux/TransactionSlice";
import React from "react";
import { Link } from "react-router-dom";

interface Props {
  transactions: Transaction[];
  filterValue: string;
}

const DynamicTable: React.FC<Props> = ({ transactions, filterValue }) => {
  return (
    <div className="border border-[#E5E7EB] rounded-xl overflow-x-auto">
      <table className="min-w-[720px] table-auto sm:table-fixed border-collapse">
        {/* Header */}
        <thead>
          <tr className="bg-[#F9FAFB]">
            <th
              className="
                w-[120px] sm:w-1/6 
                px-2 py-2 sm:px-6 sm:py-4 
                text-left text-sm sm:text-lg font-semibold text-black 
                border-b border-gray-300
              "
            >
              Transaction Date
            </th>
            <th
              className="
                w-[120px] sm:w-1/6 
                px-2 py-2 sm:px-6 sm:py-4 
                text-left text-sm sm:text-lg font-semibold text-black 
                border-b border-gray-300
              "
            >
              Transaction Fee
            </th>
            <th
              className="
                w-[120px] sm:w-1/6 
                px-2 py-2 sm:px-6 sm:py-4 
                text-left text-sm sm:text-lg font-semibold text-black 
                border-b border-gray-300
              "
            >
              {filterValue === "Plastic Credit"
                ? "Sale Amount (USD)"
                : filterValue === "Plastik"
                ? "Plastik Amount"
                : filterValue === "USDM"
                ? "USDM Amount"
                : ""}
            </th>
            <th
              className="
                w-[120px] sm:w-1/6 
                px-2 py-2 sm:px-6 sm:py-4 
                text-left text-sm sm:text-lg font-semibold text-black 
                border-b border-gray-300
              "
            >
              Token ID
            </th>
            <th
              className="
                w-[120px] sm:w-1/6 
                px-2 py-2 sm:px-6 sm:py-4 
                text-left text-sm sm:text-lg font-semibold text-black 
                border-b border-gray-300
              "
            >
              {/* Plastic Credit */}
              {filterValue}
            </th>
            <th
              className="
                w-[120px] sm:w-1/6 
                px-2 py-2 sm:px-6 sm:py-4 
                text-left text-sm sm:text-lg font-semibold text-black 
                border-b border-gray-300
              "
            >
              Hash
            </th>
          </tr>
        </thead>

        {/* Body */}
        <tbody className="divide-y divide-[#E5E7EB]">
          {transactions.map((tx) => (
            <tr key={tx.id}>
              <td className="px-2 py-2 sm:px-6 sm:py-4 whitespace-normal break-words text-md text-black">
                {new Date(tx.txDate).toLocaleDateString("en-US", {
                  day: "numeric",
                  month: "numeric",
                  year: "numeric",
                })}
                {/* {tx.txDate} */}
              </td>
              <td className="px-2 py-2 sm:px-6 sm:py-4 whitespace-normal break-words text-md text-black">
                {tx.txFee}%
              </td>
              <td className="px-2 py-2 sm:px-6 sm:py-4 whitespace-normal break-words text-md text-black">
                {tx.amount}
              </td>
              {/* Token ID: use `break-all` so even continuous digits wrap */}
              <td className="px-2 py-2 sm:px-6 sm:py-4 whitespace-normal break-all text-md text-black">
                {tx.assetId}
              </td>
              <td className="px-2 py-2 sm:px-6 sm:py-4 whitespace-normal break-words text-md">
                <Link
                  to={`https://preprod.cexplorer.io/asset/${tx.assetId}`}
                  className="text-blue-500 hover:underline"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  Link to {filterValue}
                </Link>
              </td>
              <td className="px-2 py-2 sm:px-6 sm:py-4 whitespace-normal break-words text-md">
                <Link
                  to={`https://preprod.cardanoscan.io/transaction/${tx.hash}`}
                  className="text-blue-500 hover:underline"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  Link to blockchain
                </Link>
              </td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
};

export default DynamicTable;
