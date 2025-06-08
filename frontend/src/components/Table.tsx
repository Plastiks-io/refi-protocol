import { Transaction } from "@/redux/TransactionSlice";
import React from "react";
import { Link } from "react-router-dom";

interface Props {
  transactions: Transaction[];
}

const DynamicTable: React.FC<Props> = ({ transactions }) => {
  return (
    <div className="border border-[#E5E7EB] rounded-xl overflow-x-auto">
      <table className="min-w-[720px] table-auto sm:table-fixed border-collapse">
        {/* Header */}
        <thead>
          <tr className="bg-white">
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
              Sale Amount (ADA)
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
              Plastic Credit
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
        <tbody className="divide-y divide-gray-700">
          {transactions.map((tx) => (
            <tr key={tx.id}>
              <td className="px-2 py-2 sm:px-6 sm:py-4 whitespace-normal break-words text-sm">
                {/* {new Date(tx.txDate).toLocaleDateString("en-US", {
                  day: "numeric",
                  month: "short",
                  year: "numeric",
                  hour: "2-digit",
                  minute: "2-digit",
                  second: "2-digit",
                })} */}
                {tx.txDate}
              </td>
              <td className="px-2 py-2 sm:px-6 sm:py-4 whitespace-normal break-words text-sm">
                {tx.txFee}
              </td>
              <td className="px-2 py-2 sm:px-6 sm:py-4 whitespace-normal break-words text-sm">
                {tx.amount}
              </td>
              {/* Token ID: use `break-all` so even continuous digits wrap */}
              <td className="px-2 py-2 sm:px-6 sm:py-4 whitespace-normal break-all text-sm">
                {tx.assetId}
              </td>
              <td className="px-2 py-2 sm:px-6 sm:py-4 whitespace-normal break-words text-sm">
                <Link
                  to={`https://preprod.cexplorer.io/asset/${tx.assetId}`}
                  className="text-blue-500 hover:underline"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  Link to plastic credit
                </Link>
              </td>
              <td className="px-2 py-2 sm:px-6 sm:py-4 whitespace-normal break-words text-sm">
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
