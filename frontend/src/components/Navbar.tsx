import { useState } from "react";
import { useLocation, Link } from "react-router-dom";
import Button from "./Button";
import { Menu, Settings, User } from "lucide-react";
import { PlastikLogo } from "@/assets/icons";
import { BrowserWallet, Wallet } from "@meshsdk/core";
import { useDispatch, useSelector } from "react-redux";
import { RootState } from "../redux/store";
import { setWallet, disconnectWallet } from "../redux/walletSlice";
import Popup from "./Popup";
import { toast } from "sonner";
import { clearAuthUser, setAuthUser } from "@/redux/authSlice";
import { signInOnServer, signOutOnServer } from "@/services/auth";

import DisconnectPopup from "./admin/DisconnectPopup";

const Navbar = () => {
  const dispatch = useDispatch();
  const walletAddress = useSelector(
    (state: RootState) => state.wallet.walletAddress
  );

  const role = useSelector((state: RootState) => state.auth.role);

  const isAdmin = role === "SUPER_ADMIN" || role === "ADMIN";

  const [isOpen, setIsOpen] = useState(false);
  const location = useLocation();
  const [wallets, setWallets] = useState<Wallet[]>([]);
  const [popupOpen, setPopupOpen] = useState(false);
  const [showDisconnect, setShowDisconnect] = useState(false);

  const isActive = (path: string) => location.pathname === path;
  // const adminAddress =
  //   import.meta.env.VITE_ADMIN_WALLET_ADDRESS?.toLowerCase() || "";

  const connectWallet = async () => {
    // console.log("Connecting wallet...");
    const availableWallets = await BrowserWallet.getAvailableWallets();
    // console.log(availableWallets);
    setWallets(availableWallets);
    setPopupOpen(true);
  };

  const selectWallet = async (walletId: string) => {
    console.log("Selected wallet:", walletId);
    try {
      const wallet = await BrowserWallet.enable(walletId);
      const address = (await wallet.getChangeAddress()) || "N/A";
      dispatch(setWallet({ walletId, address }));

      // ─── SIGN IN ON YOUR SERVER ─────────────────────────────
      // call your /auth/signin endpoint so you get the JWT cookie
      const data = await signInOnServer(address);
      dispatch(setAuthUser({ email: data.email, role: data.role }));
      setPopupOpen(false);
    } catch (error) {
      if (error instanceof Error) {
        console.log(error);
        toast.error("Please Activate Connect as DApp Account in your wallet");
      } else {
        toast.error("An unknown error occurred");
      }
    }
  };

  const disconnect = async () => {
    try {
      await signOutOnServer();
    } catch (err) {
      toast.error("Failed to sign out from server");
    }
    dispatch(disconnectWallet());
    dispatch(clearAuthUser());
  };

  return (
    <nav className="bg-white shadow-md w-full sticky top-0 z-50">
      <div className="flex items-center justify-between max-w-7xl mx-auto px-6 py-4">
        {/* Left Side: Logo + Links */}
        <div className="flex items-center space-x-12">
          {/* Logo */}
          <div className="flex flex-col">
            <img src={PlastikLogo} alt="Plastiks" className="h-12 w-auto" />
            <span className="self-end text-lg text-[#4F4F4F]">ReFi Dapp</span>
          </div>

          {/* Desktop Menu */}
          <div className="hidden md:flex space-x-8">
            <Link
              to="/"
              className={`${
                isActive("/")
                  ? "text-[#0D0D0D] font-semibold text-xl border-b-2"
                  : "text-gray-500 text-xl"
              } hover:text-blue-600 transition`}
            >
              Roadmaps
            </Link>
            <Link
              to="/transactions"
              className={`${
                isActive("/transactions")
                  ? "text-[#0D0D0D] font-semibold text-xl border-b-2"
                  : "text-gray-500 text-xl"
              } hover:text-blue-600 transition`}
            >
              Transactions
            </Link>
            <Link
              to="/community"
              className={`${
                isActive("/community")
                  ? "text-[#0D0D0D] font-semibold text-xl border-b-2"
                  : "text-gray-500 text-xl"
              } hover:text-blue-600 transition`}
            >
              Community
            </Link>
            <Link
              to="/buy-nft"
              className={`${
                isActive("/buy-nft")
                  ? "text-[#0D0D0D] font-semibold text-xl border-b-2"
                  : "text-gray-500 text-xl"
              } hover:text-blue-600 transition`}
            >
              Buy NFT
            </Link>
            <Link
              to="/lend"
              className={`${
                isActive("/lend")
                  ? "text-[#0D0D0D] font-semibold text-xl border-b-2"
                  : "text-gray-500 text-xl"
              } hover:text-blue-600 transition`}
              style={{
                display: walletAddress ? "block" : "none",
              }}
            >
              Lend
            </Link>
            <Link
              to="/admin"
              className={`${
                isActive("/admin")
                  ? "text-[#0D0D0D] font-semibold text-xl border-b-2"
                  : "text-gray-500 text-xl"
              } hover:text-blue-600 transition`}
              style={{
                display:
                  role === "ADMIN" || role === "SUPER_ADMIN" ? "block" : "none",
              }}
            >
              Admin
            </Link>
          </div>
        </div>

        {/* Right Side: Connect Wallet Button */}
        <div className="flex items-center space-x-4">
          <div className="hidden md:block">
            {!walletAddress ? (
              <Button
                variant="userButton"
                onClick={connectWallet}
                className="bg-[#082FB9] text-white rounded-full font-semibold"
              >
                Connect Wallet
              </Button>
            ) : isAdmin ? (
              <div className="flex items-center space-x-4">
                {/* Settings link */}
                <Link
                  className="w-10 h-10 flex items-center justify-center rounded-full bg-gray-100 hover:bg-gray-200"
                  to="admin/settings"
                >
                  <Settings className="w-5 h-5 text-black" />
                </Link>

                {/* Avatar – click to open disconnect */}
                <div
                  className="w-10 h-10 rounded-full bg-[#082FB9] flex items-center justify-center cursor-pointer"
                  onClick={() => setShowDisconnect(true)}
                >
                  <User />
                </div>

                {/* Confirmation popup */}
                <DisconnectPopup
                  isOpen={showDisconnect}
                  onConfirm={() => {
                    disconnect();
                    setShowDisconnect(false);
                  }}
                  onCancel={() => setShowDisconnect(false)}
                />
              </div>
            ) : (
              <div className="flex items-center space-x-4">
                <span className="text-gray-800 font-medium bg-gray-100 px-3 py-1 rounded-lg">
                  {walletAddress.slice(0, 6)}...{walletAddress.slice(-4)}
                </span>
                <button
                  onClick={disconnect}
                  className="text-red-600 hover:text-red-800 text-sm font-medium"
                >
                  Disconnect
                </button>
              </div>
            )}
          </div>

          {/* Mobile Menu Button */}
          <div className="md:hidden">
            <button
              onClick={() => setIsOpen(!isOpen)}
              data-testid="menu-button"
              aria-label="Open mobile menu"
              className="p-2 rounded-md text-gray-500 hover:text-gray-900"
            >
              <Menu size={24} />
            </button>
          </div>
        </div>
      </div>

      {/* Mobile Menu */}
      {isOpen && (
        <div
          className="md:hidden bg-white shadow-lg p-4"
          data-testid="mobile-menu"
        >
          <div className="flex flex-col items-center space-y-4">
            <Link
              to="/"
              className={`${
                isActive("/")
                  ? "text-[#0D0D0D] font-semibold text-xl border-b-2"
                  : "text-gray-500 text-xl"
              } hover:text-blue-600 transition`}
            >
              Roadmaps
            </Link>
            <Link
              to="/transactions"
              className={`${
                isActive("/transactions")
                  ? "text-[#0D0D0D] font-semibold text-xl border-b-2"
                  : "text-gray-500 text-xl"
              } hover:text-blue-600 transition`}
            >
              Transactions
            </Link>
            <Link
              to="/community"
              className={`${
                isActive("/community")
                  ? "text-[#0D0D0D] font-semibold text-xl border-b-2"
                  : "text-gray-500 text-xl"
              } hover:text-blue-600 transition`}
            >
              Community
            </Link>
            <Link
              to="/buy-nft"
              className={`${
                isActive("/buy-nft")
                  ? "text-[#0D0D0D] font-semibold text-xl border-b-2"
                  : "text-gray-500 text-xl"
              } hover:text-blue-600 transition`}
            >
              Buy NFT
            </Link>
            <Link
              to="/admin"
              className={`${
                isActive("/admin")
                  ? "text-[#0D0D0D] font-semibold text-xl border-b-2"
                  : "text-gray-500 text-xl"
              } hover:text-blue-600 transition`}
              style={{
                display:
                  role === "ADMIN" || role === "SUPER_ADMIN" ? "block" : "none",
              }}
            >
              Admin
            </Link>
            <div className="flex items-center justify-center">
              {!walletAddress ? (
                <Button
                  variant="userButton"
                  onClick={connectWallet}
                  className="bg-[#082FB9] text-white rounded-full font-semibold"
                >
                  Connect Wallet
                </Button>
              ) : isAdmin ? (
                <div
                  className="flex items-center space-x-4 cursor-pointer"
                  onClick={() => setShowDisconnect(true)}
                >
                  {/* Settings Icon navigate to setting*/}
                  <Link
                    className="w-10 h-10 flex items-center justify-center rounded-full bg-gray-100 hover:bg-gray-200 cursor-pointer"
                    to={"admin/settings"}
                  >
                    <Settings className="w-5 h-5 text-black" />
                  </Link>

                  <div className="w-10 h-10 rounded-full bg-[#082FB9] flex items-center justify-center">
                    <User />
                  </div>
                </div>
              ) : (
                <div className="flex items-center space-x-4">
                  <span className="text-gray-800 font-medium bg-gray-100 px-3 py-1 rounded-lg">
                    {walletAddress.slice(0, 6)}...{walletAddress.slice(-4)}
                  </span>
                  <button
                    onClick={disconnect}
                    className="text-red-600 hover:text-red-800 text-sm font-medium"
                  >
                    Disconnect
                  </button>
                </div>
              )}
            </div>
          </div>
        </div>
      )}

      {/* Popup for Wallet Selection */}
      {popupOpen && (
        <Popup
          wallets={wallets}
          onSelectWallet={selectWallet}
          onClose={() => setPopupOpen(false)}
        />
      )}
    </nav>
  );
};

export default Navbar;
