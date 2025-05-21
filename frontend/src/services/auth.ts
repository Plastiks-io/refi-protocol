import axios from "axios";
import { toast } from "sonner";

const apiClient = axios.create({
  baseURL: import.meta.env.VITE_SERVER_URL,
  withCredentials: true,
});

export const signInOnServer = async (address: string) => {
  try {
    const res = await apiClient.post("/auth/signin", { address });

    if (!res.data || !res.data.success) {
      throw new Error(res.data?.message || "Sign-in failed");
    }
    toast.success("Signed in successfully");
    return res.data;
  } catch (err: any) {
    toast.error("Sign-in failed");
    throw new Error(
      err.response?.data?.message || err.message || "Sign-in error"
    );
  }
};

export const signOutOnServer = async () => {
  try {
    const res = await apiClient.post("/auth/signout");

    if (!res.data.success) {
      throw new Error(res.data.message || "Sign-out failed");
    }
    toast.success("Signed out successfully");
  } catch (err: any) {
    toast.error("Sign-out failed");
    throw new Error(
      err.response?.data?.message || err.message || "Sign-out error"
    );
  }
};
