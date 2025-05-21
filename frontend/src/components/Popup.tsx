import React from "react";
import { X } from "lucide-react";

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
    <div className="fixed inset-0 bg-[rgba(0,0,0,0.5)] flex justify-center items-center z-50 md:justify-end md:items-start">
      <div className="bg-white text-gray-700 rounded-2xl shadow-lg p-6 w-full max-w-md mx-4 my-8 md:mx-0 md:my-0 md:w-96 md:mt-30 md:mr-20">
        <div className="flex justify-between items-center">
          <h2 className="text-xl font-bold mb-6">Connect Wallet</h2>
          <X
            onClick={onClose}
            color="#364153"
            strokeWidth={1.5}
            size={24}
            className="cursor-pointer"
          />
        </div>
        <div className="flex flex-col gap-3">
          {wallets.map((wallet) => (
            <div
              key={wallet.id}
              className="flex items-center gap-4 p-2 rounded-2xl shadow-sm hover:shadow-md transition-shadow cursor-pointer border border-[#D7C1B3]"
              onClick={() => onSelectWallet(wallet.id)}
            >
              <img
                src={wallet.icon}
                alt={wallet.name}
                className="w-10 h-10 rounded-full object-cover"
              />
              <p className="font-semibold text-lg">{wallet.name}</p>
            </div>
          ))}
        </div>
      </div>
    </div>
  );
};

export default Popup;
