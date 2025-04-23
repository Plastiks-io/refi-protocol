import React from "react";

interface Transaction {
  date: string;
  transactionFee: string;
  amount: string;
  tokenId: string;
  plasticCredit: string;
  hash: string;
}

interface Props {
  transactions: Transaction[];
}

const DynamicTable: React.FC<Props> = ({ transactions }) => {
  if (!transactions || transactions.length === 0) {
    return (
      <p className="text-gray-600 text-center mt-4">
        No transactions available.
      </p>
    );
  }

  return (
    <div className="overflow-x-auto rounded-lg">
      <table className="min-w-full bg-white border border-gray-200 shadow-md">
        <thead>
          <tr className="bg-gray-100">
            {Object.keys(transactions[0]).map((key) => (
              <th
                key={key}
                className="px-6 py-3 text-left text-l font-semibold text-black capitalize"
              >
                {key.replace(/([A-Z])/g, " $1")} {/* Format camelCase keys */}
              </th>
            ))}
          </tr>
        </thead>
        <tbody>
          {transactions.map((tx, index) => (
            <tr key={index} className="border-t border-gray-200">
              {Object.values(tx).map((value, i) => (
                <td key={i} className="px-6 py-4 text-gray-700">
                  {typeof value === "string" && value.startsWith("http") ? (
                    <a href={value} className="text-blue-600 underline">
                      {value}
                    </a>
                  ) : (
                    value
                  )}
                </td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
};

export default DynamicTable;
