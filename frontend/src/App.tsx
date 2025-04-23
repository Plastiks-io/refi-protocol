import "./App.css";
import Dashboard from "./pages/dashboard";
import Navbar from "./components/Navbar";
import Footer from "./components/Footer";
import Transactions from "./pages/dashboard/transactions";
import Community from "./pages/dashboard/community";
import { BrowserRouter as Router, Route, Routes } from "react-router-dom";
import { Toaster } from "sonner";
import PrivateAdminRoute from "./components/PrivateAdminRoute";
import Admin from "./pages/dashboard/admin";
import NFTPurchase from "./pages/user/nft";

// App.tsx or WalletProvider.tsx
import React, { useEffect, useState } from "react";
import { useSelector, useDispatch } from "react-redux";
import { AppDispatch } from "./redux/store";
import { BrowserWallet } from "@meshsdk/core";
import { RootState } from "./redux/store";
import { fetchRoadmaps } from "./redux/roadmapSlice";
import { fetchCompletedRoadmaps } from "./redux/completedRoadmapSlice";
import RoadmapDetails from "./components/admin/RoadmapDetails";

// useEffect(() => {

// }, []);

export const WalletContext = React.createContext<BrowserWallet | null>(null);

function App() {
  const walletId = useSelector(
    (state: RootState) => state.wallet.walletId
  ) as string;

  const [wallet, setWallet] = useState<BrowserWallet | null>(null);
  const dispatch = useDispatch<AppDispatch>();

  const reconnectWallet = async () => {
    if (!walletId) {
      setWallet(null); // This line is important to clear context
      return;
    }

    const availableWallets = await BrowserWallet.getAvailableWallets();
    const selectedWallet = availableWallets.find(
      (wallet) => wallet.id === walletId
    );

    if (!selectedWallet) {
      console.log(`Wallet with ID ${walletId} not found.`);
      setWallet(null); // 👈 Also nullify here in fallback
      return;
    }

    const connectedWallet = await BrowserWallet.enable(walletId);
    setWallet(connectedWallet);
  };

  // Use useEffect to reconnect the wallet when the component mounts or when walletId changes
  useEffect(() => {
    reconnectWallet();
    dispatch(fetchRoadmaps());
    dispatch(fetchCompletedRoadmaps());
  }, [walletId]);

  return (
    <WalletContext.Provider value={wallet}>
      <Router>
        <div className="flex flex-col min-h-screen">
          {/* Navbar at the top */}
          <Navbar />

          {/* Main content area */}
          <main className="flex-grow">
            <Routes>
              <Route path="/" element={<Dashboard />} />
              <Route path="/transactions" element={<Transactions />} />
              <Route path="/community" element={<Community />} />
              <Route
                path="/admin"
                element={
                  <PrivateAdminRoute>
                    <Admin />
                  </PrivateAdminRoute>
                }
              />
              <Route path="/buy-nft" element={<NFTPurchase />} />
              <Route
                path="admin/roadmap/:roadmapId"
                element={<RoadmapDetails />}
              />
            </Routes>
          </main>

          {/* Footer at the bottom */}
          <Footer />
        </div>
        <Toaster position="top-right" />
      </Router>
    </WalletContext.Provider>
  );
}

export default App;
