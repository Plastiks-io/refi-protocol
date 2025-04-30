import { useState } from "react";
import { useLocation, Link } from "react-router-dom";
import Button from "./Button";
import { Menu } from "lucide-react";
import PlastikLogo from "../assets/plastik_logo.svg";
import { faWallet } from "@fortawesome/free-solid-svg-icons";
import { BrowserWallet, Wallet } from "@meshsdk/core";
import { useDispatch, useSelector } from "react-redux";
import { RootState } from "../redux/store";
import { setWallet, disconnectWallet } from "../redux/walletSlice";
import Popup from "./Popup";
import { toast } from "sonner";
import { resetTransactions } from "../redux/TransactionSlice";

const Navbar = () => {
  const dispatch = useDispatch();
  const walletAddress = useSelector(
    (state: RootState) => state.wallet.walletAddress
  );

  const [isOpen, setIsOpen] = useState(false);
  const location = useLocation();
  const [wallets, setWallets] = useState<Wallet[]>([]);
  const [popupOpen, setPopupOpen] = useState(false);

  const isActive = (path: string) => location.pathname === path;
  const adminAddress = import.meta.env.VITE_ADMIN_WALLET_ADDRESS?.toLowerCase() || "";

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

  return (
    <nav className="bg-white shadow-md w-full sticky top-0 z-50">
      <div className="flex items-center justify-between max-w-7xl mx-auto px-6 py-4">
        {/* Left Side: Logo + Links */}
        <div className="flex items-center space-x-12">
          {/* Logo */}
          <div className="flex items-center space-x-2">
            <img src={PlastikLogo} alt="Plastiks" className="h-15 w-auto" />
            <div className="flex flex-col leading-tight">
              <span className="text-2xl font-bold text-gray-900">Plastiks</span>
              <span className="ml-5 text-sm text-gray-500">ReFi Dapp</span>
            </div>
          </div>

          {/* Desktop Menu */}
          <div className="hidden md:flex space-x-8">
            <Link
              to="/"
              className={`${
                isActive("/") ? "text-gray-800 font-semibold" : "text-gray-500"
              } hover:text-blue-600 transition`}
            >
              Roadmaps
            </Link>
            <Link
              to="/transactions"
              className={`${
                isActive("/transactions")
                  ? "text-gray-800 font-semibold"
                  : "text-gray-500"
              } hover:text-blue-600 transition`}
            >
              Transactions
            </Link>
            <Link
              to="/community"
              className={`${
                isActive("/community")
                  ? "text-gray-800 font-semibold"
                  : "text-gray-500"
              } hover:text-blue-600 transition`}
            >
              Community
            </Link>
            <Link
              to="/buy-nft"
              className={`${
                isActive("/buy-nft")
                  ? "text-gray-800 font-semibold"
                  : "text-gray-500"
              } hover:text-blue-600 transition`}
            >
              Buy NFT
            </Link>
            <Link
              to="/admin"
              className={`${
                isActive("/admin")
                  ? "text-gray-800 font-semibold"
                  : "text-gray-500"
              } hover:text-blue-600 transition`}
              style={{
                display:
                  walletAddress?.toLowerCase() === adminAddress
                    ? "block"
                    : "none",
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
              <Button variant="gray" icon={faWallet} onClick={connectWallet}>
                Connect Wallet
              </Button>
            ) : (
              <div className="flex items-center space-x-4">
                <span className="text-gray-800 font-medium bg-gray-100 px-3 py-1 rounded-lg">
                  {walletAddress.slice(0, 6)}...{walletAddress.slice(-4)}
                </span>
                <button
                  onClick={() => {
                    dispatch(disconnectWallet());
                    dispatch(resetTransactions());
                  }}
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
              className={`block w-full text-center py-2 ${
                isActive("/") ? "text-blue-600 font-bold" : "text-gray-500"
              } hover:text-blue-600 transition`}
            >
              Roadmaps
            </Link>
            <Link
              to="/transactions"
              className={`block w-full text-center py-2 ${
                isActive("/transactions")
                  ? "text-blue-600 font-bold"
                  : "text-gray-500"
              } hover:text-blue-600 transition`}
            >
              Transactions
            </Link>
            <Link
              to="/community"
              className={`block w-full text-center py-2 ${
                isActive("/community")
                  ? "text-blue-600 font-bold"
                  : "text-gray-500"
              } hover:text-blue-600 transition`}
            >
              Community
            </Link>
            <Link
              to="/admin"
              className={`${
                isActive("/admin")
                  ? "text-gray-800 font-semibold"
                  : "text-gray-500"
              } hover:text-blue-600 transition`}
              style={{
                display:
                  walletAddress?.toLowerCase() === adminAddress
                    ? "block"
                    : "none",
              }}
            >
              Admin
            </Link>
            <div className="flex items-center justify-center">
              {!walletAddress ? (
                <Button variant="gray" icon={faWallet} onClick={connectWallet}>
                  Connect Wallet
                </Button>
              ) : (
                <div className="flex items-center space-x-4">
                  <span className="text-gray-800 font-medium bg-gray-100 px-3 py-1 rounded-lg">
                    {walletAddress.slice(0, 6)}...{walletAddress.slice(-4)}
                  </span>
                  <button
                    onClick={() => dispatch(disconnectWallet())}
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
