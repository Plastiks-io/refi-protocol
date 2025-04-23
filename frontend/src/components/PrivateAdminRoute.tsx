import { Navigate } from "react-router-dom";
import { useSelector } from "react-redux";
import { RootState } from "../redux/store";
import React from "react";

interface Props {
  children: React.ReactNode;
}

const PrivateAdminRoute = ({ children }: Props) => {
  const walletAddress = useSelector(
    (state: RootState) => state.wallet.walletAddress
  );
  const adminAddress = import.meta.env.VITE_ADMIN_WALLET_ADDRESS?.toLowerCase();

  if (walletAddress?.toLowerCase() !== adminAddress) {
    return <Navigate to="/" replace />;
  }

  return <>{children}</>;
};

export default PrivateAdminRoute;
