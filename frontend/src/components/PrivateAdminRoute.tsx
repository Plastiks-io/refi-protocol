import { Navigate } from "react-router-dom";
import { useSelector } from "react-redux";
import { RootState } from "../redux/store";
import React from "react";

interface Props {
  children: React.ReactNode;
}

const PrivateAdminRoute = ({ children }: Props) => {
  const role = useSelector((state: RootState) => state.auth.role);

  const isAdmin = role === "SUPER_ADMIN" || role === "ADMIN";
  if (!isAdmin) {
    return <Navigate to="/" replace />;
  }

  return <>{children}</>;
};

export default PrivateAdminRoute;
