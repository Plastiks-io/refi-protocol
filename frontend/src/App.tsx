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
import RoadmapDetails from "./components/admin/RoadmapDetails";

// App.tsx or WalletProvider.tsx
import React, { useEffect, useState } from "react";
import { useSelector, useDispatch } from "react-redux";
import { AppDispatch } from "./redux/store";
import { BrowserWallet } from "@meshsdk/core";
import { RootState } from "./redux/store";
import { fetchRoadmaps } from "./redux/roadmapSlice";
import { fetchCompletedRoadmaps } from "./redux/completedRoadmapSlice";
import { fetchArchivedRoadmaps } from "./redux/archivedRoadmapSlice";
import { fetchTransactions, TransactionType } from "./redux/TransactionSlice";
import Settings from "./components/admin/Setting";
import { fetchAdmins } from "./redux/adminSlice";
import Lend from "@/pages/dashboard/lend";
import PrivateLendRoute from "./components/PrivateLendRoute";
import { CardanoProvider } from "@/contexts/cardanoContexts";
import SocketManager from "./socket/SocketManager";
export const WalletContext = React.createContext<BrowserWallet | null>(null);

function App() {
  const { walletId } = useSelector((state: RootState) => state.wallet);
  // on‑chain values
  const { transactions, loading, error, page, perPage, totalPages } =
    useSelector((state: RootState) => state.transactions);

  // Page change handler
  const handlePageChange = (
    newPage: number,
    filterType: TransactionType | TransactionType[]
  ) => {
    dispatch(fetchTransactions({ page: newPage, perPage, type: filterType }));
  };

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
      setWallet(null);
      return;
    }

    const connectedWallet = await BrowserWallet.enable(walletId);
    setWallet(connectedWallet);
  };

  // Use useEffect to reconnect the wallet when the component mounts or when walletId changes
  const role = useSelector((state: RootState) => state.auth.role);
  const isAdmin = role === "SUPER_ADMIN" || role === "ADMIN";
  useEffect(() => {
    reconnectWallet();
    dispatch(fetchRoadmaps());
    dispatch(fetchCompletedRoadmaps());
    dispatch(fetchArchivedRoadmaps());
    // only fetch admins if user is already an admin
    if (isAdmin) {
      dispatch(fetchAdmins());
    }
  }, [walletId, dispatch]);

  return (
    <WalletContext.Provider value={wallet}>
      <CardanoProvider wallet={wallet}>
        <Router>
          <div className="flex flex-col min-h-screen">
            {/* Socket Manager - handles socket events */}
            <SocketManager />
            {/* Navbar at the top */}
            <Navbar />

            {/* Main content area */}
            <main className="flex-grow">
              <Routes>
                <Route path="/" element={<Dashboard />} />
                {/* <Route path="/transactions" element={<Transactions />} /> */}
                <Route
                  path="/transactions"
                  element={
                    <Transactions
                      transactions={transactions}
                      loading={loading}
                      error={error}
                      page={page}
                      totalPages={totalPages}
                      onPageChange={handlePageChange}
                    />
                  }
                />
                <Route path="/community" element={<Community />} />
                <Route
                  path="/admin"
                  element={
                    <PrivateAdminRoute>
                      <Admin />
                    </PrivateAdminRoute>
                  }
                />
                <Route
                  path="/admin/settings"
                  element={
                    <PrivateAdminRoute>
                      <Settings />
                    </PrivateAdminRoute>
                  }
                />
                <Route path="/buy-nft" element={<NFTPurchase />} />

                <Route
                  path="/lend"
                  element={
                    <PrivateLendRoute>
                      <Lend />
                    </PrivateLendRoute>
                  }
                />

                <Route
                  path="/roadmap/:roadmapId"
                  element={<RoadmapDetails />}
                />
              </Routes>
            </main>

            {/* Footer at the bottom */}
            <Footer />
          </div>
          <Toaster position="top-right" />
        </Router>
      </CardanoProvider>
    </WalletContext.Provider>
  );
}

export default App;
