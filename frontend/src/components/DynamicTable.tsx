import { Transaction } from "@/redux/TransactionSlice";
import React from "react";
import { Link } from "react-router-dom";
import { toast } from "sonner";

interface Props {
  transactions: Transaction[];
  filterValue: string;
}

const DynamicTable: React.FC<Props> = ({ transactions, filterValue }) => {
  const copyToClipboard = (e: React.MouseEvent<HTMLDivElement>) => {
    try {
      const text = e.currentTarget.textContent || "";
      navigator.clipboard.writeText(text).then(() => {
        console.log("Copied to clipboard:", text);
      });
      toast.success("Copied to clipboard!", {
        closeButton: true,
      });
    } catch (error) {
      console.error("Failed to copy text:", error);
      toast.error("Failed to copy text", {
        closeButton: true,
      });
    }
  };
  return (
    <div className="w-full border border-[#E5E7EB] rounded-xl overflow-x-auto">
      <table className="w-full min-w-[800px] table-fixed border-collapse">
        {/* enforce 6 equal columns with minimum widths */}
        <colgroup>
          <col className="w-1/6 min-w-[120px]" />
          <col className="w-1/6 min-w-[100px]" />
          <col className="w-1/6 min-w-[120px]" />
          <col className="w-1/6 min-w-[140px]" />
          <col className="w-1/6 min-w-[120px]" />
          <col className="w-1/6 min-w-[140px]" />
        </colgroup>

        <thead>
          <tr className="bg-[#F9FAFB] h-16">
            <th className="px-2 py-4 text-left text-sm sm:text-lg font-semibold text-black border-b border-gray-300 whitespace-nowrap">
              Transaction Date
            </th>
            <th className="px-2 py-4 text-left text-sm sm:text-lg font-semibold text-black border-b border-gray-300 whitespace-nowrap">
              Transaction Fee
            </th>
            <th className="px-2 py-4 text-left text-sm sm:text-lg font-semibold text-black border-b border-gray-300 whitespace-nowrap">
              {filterValue === "Plastic Credit"
                ? "Sale Amount (USD)"
                : filterValue === "Plastik"
                ? "Plastik Amount"
                : filterValue === "USDM"
                ? "USDM Amount"
                : "Amount"}
            </th>
            <th className="px-2 py-4 text-left text-sm sm:text-lg font-semibold text-black border-b border-gray-300 whitespace-nowrap">
              Token ID
            </th>
            <th className="px-2 py-4 text-left text-sm sm:text-lg font-semibold text-black border-b border-gray-300 whitespace-nowrap">
              {filterValue}
            </th>
            <th className="px-2 py-4 text-left text-sm sm:text-lg font-semibold text-black border-b border-gray-300 whitespace-nowrap">
              Hash
            </th>
          </tr>
        </thead>

        <tbody className="divide-y divide-[#E5E7EB]">
          {transactions.map((tx) => (
            <tr key={tx.id}>
              <td className="px-2 py-4 whitespace-nowrap text-md text-black">
                {new Date(tx.txDate).toLocaleDateString("en-US", {
                  day: "numeric",
                  month: "numeric",
                  year: "numeric",
                })}
              </td>
              <td className="px-2 py-4 whitespace-nowrap text-md text-black">
                {tx.txFee}%
              </td>
              <td className="px-2 py-4 whitespace-nowrap text-md text-black">
                {tx.amount}
              </td>
              <td className="px-2 py-4 text-md text-black">
                <div
                  className="truncate cursor-pointer"
                  title={tx.assetId}
                  onClick={copyToClipboard}
                >
                  {tx.assetId}
                </div>
              </td>
              <td className="px-2 py-4 whitespace-nowrap text-md">
                <Link
                  to={`https://preprod.cexplorer.io/asset/${tx.assetId}`}
                  className="text-blue-500 hover:underline"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  Link to {filterValue}
                </Link>
              </td>
              <td className="px-2 py-4 whitespace-nowrap text-md">
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
