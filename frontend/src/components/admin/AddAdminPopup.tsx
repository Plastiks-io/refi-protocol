// src/components/admin/AddAdminPopup.tsx
import React, { useState, ChangeEvent, FormEvent } from "react";
import { X, User as UserIcon } from "lucide-react";
import axios from "axios";
import { useDispatch } from "react-redux";
import { addAdmin } from "@/redux/adminSlice";
import { toast } from "sonner";
import { isValidCardanoAddress, truncateAddress } from "@/utils/helper";

interface ExistingAdmin {
  address: string;
  isCurrent?: boolean;
}

interface AddAdminPopupProps {
  isOpen: boolean;
  onClose: () => void;
  existingAdmins: ExistingAdmin[];
  walletAddress: string | null;
}

const AddAdminPopup: React.FC<AddAdminPopupProps> = ({
  isOpen,
  onClose,
  existingAdmins,
  walletAddress,
}) => {
  const dispatch = useDispatch();
  const [walletInput, setWalletInput] = useState("");

  if (!isOpen) return null;

  const handleChange = (e: ChangeEvent<HTMLInputElement>) => {
    setWalletInput(e.target.value);
  };

  const handleSubmit = async (e: FormEvent) => {
    e.preventDefault();
    if (!isValidCardanoAddress(walletInput)) {
      toast.error("Invalid Cardano address", {
        closeButton: true,
      });
      setWalletInput("");
      return;
    }
    if (walletInput.trim() !== "") {
      try {
        const url = import.meta.env.VITE_SERVER_URL;
        const apiUrl = `${url}/admin/`;
        const response = await axios.post(
          apiUrl,
          {
            address: walletInput,
          },
          {
            withCredentials: true,
          }
        );

        // also remove from redux state
        dispatch(addAdmin(response.data.admin));
        console.log(response.data);
        toast.success("Admin added successfully", {
          closeButton: true,
        });
      } catch (error) {
        toast.error("Failed to add admin", {
          closeButton: true,
        });
        console.error("Error adding admin:", error);
      }
      setWalletInput("");
    } else {
      toast.error("Please enter a valid wallet address", {
        closeButton: true,
      });
    }
  };

  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center bg-[rgba(0,0,0,0.5)]">
      <div className="bg-white rounded-2xl shadow-lg w-11/12 max-w-md p-6">
        {/* Header with centered title and “X” at the end */}
        <div className="relative mb-10">
          {/* Centered Title */}
          <h2 className="absolute left-1/2 top-0 transform -translate-x-1/2 text-2xl font-semibold">
            Add New Admin
          </h2>

          {/* “X” Button in the top-right corner */}
          <button
            className="absolute right-0 top-0 p-1 rounded-full hover:bg-gray-200"
            onClick={onClose}
          >
            <X className="w-5 h-5 text-[#325DF6]" />
          </button>
        </div>

        {/* Subtitle */}
        <p className="text-[#000000] text-md mb-4">
          Enter Recipient&apos;s Wallet Address to Confirm
        </p>

        {/* Input Field */}
        <form onSubmit={handleSubmit}>
          <input
            type="text"
            placeholder="addr…7b21"
            value={walletInput}
            onChange={handleChange}
            className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 mb-6"
          />

          {/* Existing Admins List */}
          <div className="space-y-3 max-h-48 overflow-y-auto mb-6">
            {existingAdmins.map((admin, idx) => (
              <div key={idx} className="flex items-center justify-between py-2">
                <div className="flex items-center space-x-3">
                  <div className="bg-[#082FB9] rounded-full p-2">
                    <UserIcon className="w-5 h-5 text-white" />
                  </div>
                  <span className="text-gray-700">
                    {truncateAddress(admin.address)}
                  </span>
                </div>
                {admin.address === walletAddress && (
                  <span className="text-sm text-gray-500">You</span>
                )}
              </div>
            ))}
          </div>

          {/* Invite Button */}
          <div className="flex justify-center">
            <button
              type="submit"
              className="bg-[#082FB9] text-white font-semibold py-2 px-4 rounded-full hover:bg-blue-700"
            >
              Invite Admin
            </button>
          </div>
        </form>
      </div>
    </div>
  );
};

export default AddAdminPopup;
