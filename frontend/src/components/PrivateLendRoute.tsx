import { Navigate } from "react-router-dom";
import { useSelector } from "react-redux";
import { RootState } from "../redux/store";
import React from "react";

interface Props {
  children: React.ReactNode;
}

const PrivateLendRoute = ({ children }: Props) => {
  const walletAddress = useSelector(
    (state: RootState) => state.wallet.walletAddress
  );
  if (!walletAddress) {
    return <Navigate to="/" replace />;
  }

  return <>{children}</>;
};

export default PrivateLendRoute;
