// src/components/DisconnectPopup.tsx
import React from "react";

interface DisconnectPopupProps {
  isOpen: boolean;
  onConfirm: () => void;
  onCancel: () => void;
}

const DisconnectPopup: React.FC<DisconnectPopupProps> = ({
  isOpen,
  onConfirm,
  onCancel,
}) => {
  if (!isOpen) return null;

  return (
    <div className="fixed inset-0 bg-[rgba(0,0,0,0.5)] flex justify-center items-center z-100 md:justify-end md:items-start">
      <div className="bg-white text-gray-700 rounded-2xl shadow-lg p-6 w-full max-w-md mx-4 my-8 md:mx-0 md:my-0 md:w-96 md:mt-30 md:mr-20">
        <h2 className="text-xl font-semibold mb-4 text-center text-gray-800">
          Disconnect Wallet
        </h2>
        <p className="text-gray-600 mb-6 text-center">
          Are you sure you want to disconnect your wallet?
        </p>
        <div className="flex justify-around space-x-4">
          <button
            onClick={onCancel}
            className="px-4 py-2 rounded-lg bg-gray-200 text-gray-800 hover:bg-gray-300"
          >
            Cancel
          </button>
          <button
            onClick={onConfirm}
            className="px-4 py-2 rounded-lg bg-red-600 text-white hover:bg-red-700"
          >
            Disconnect
          </button>
        </div>
      </div>
    </div>
  );
};

export default DisconnectPopup;
