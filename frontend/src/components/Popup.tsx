import React from "react";

type Wallet = {
  icon: string;
  id: string;
  name: string;
  version: string;
};

type PopupProps = {
  wallets: Wallet[];
  onSelectWallet: (walletId: string) => void;
  onClose: () => void;
};

const Popup: React.FC<PopupProps> = ({ wallets, onSelectWallet, onClose }) => {
  return (
    <div className="fixed inset-0 bg-[rgba(0,0,0,0.7)] flex justify-center items-center z-50">
      <div className="bg-white text-gray-700 rounded-lg shadow-lg p-6 w-96">
        <h2 className="text-xl font-bold mb-6 text-center">Select a Wallet</h2>
        <div className="flex flex-col gap-6">
          {wallets.map((wallet) => (
            <div
              key={wallet.id}
              className="flex items-center gap-4 p-4 rounded-lg shadow-sm hover:shadow-md transition-shadow cursor-pointer"
              onClick={() => onSelectWallet(wallet.id)}
            >
              <img
                src={wallet.icon}
                alt={wallet.name}
                className="w-12 h-12 rounded-full object-cover"
              />
              <div>
                <p className="font-semibold text-lg">{wallet.name}</p>
                <p className="text-sm text-gray-500">v{wallet.version}</p>
              </div>
            </div>
          ))}
        </div>
        <button
          className="mt-6 w-full bg-gray-800 text-white py-2 rounded-lg hover:bg-gray-700 transition-colors"
          onClick={onClose}
        >
          Close
        </button>
      </div>
    </div>
  );
};

export default Popup;