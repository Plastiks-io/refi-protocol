import {  createSlice, PayloadAction } from "@reduxjs/toolkit";

interface AuthState {
  role: "SUPER_ADMIN" | "ADMIN" | "USER" | null;
  isAuthenticated: boolean;
}

const initialState: AuthState = {
  role: null,
  isAuthenticated: false,
};

export const authSlice = createSlice({
  name: "auth",
  initialState,
  reducers: {
    setAuthUser: (
      state,
      action: PayloadAction<{
        email?: string;
        role: "SUPER_ADMIN" | "ADMIN" | "USER";
      }>
    ) => {
      state.role = action.payload.role;
      state.isAuthenticated = true;
    },
    clearAuthUser: (state) => {
      state.role = null;
      state.isAuthenticated = false;
    },
  },
});

export const { setAuthUser, clearAuthUser } = authSlice.actions;
export default authSlice.reducer;
