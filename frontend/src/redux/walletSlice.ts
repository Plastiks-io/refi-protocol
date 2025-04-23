import { createSlice, PayloadAction } from "@reduxjs/toolkit";

type WalletState = {
  walletId: string | null;
  walletAddress: string | null;
};

const initialState: WalletState = {
  walletId: null,
  walletAddress: null,
};

const walletSlice = createSlice({
  name: "wallet",
  initialState,
  reducers: {
    setWallet: (
      state,
      action: PayloadAction<{ walletId: string; address: string }>
    ) => {
      state.walletId = action.payload.walletId;
      state.walletAddress = action.payload.address;
    },
    disconnectWallet: (state) => {
      state.walletId = null;
      state.walletAddress = null;
    },
  },
});

export const { setWallet, disconnectWallet } = walletSlice.actions;
export default walletSlice.reducer;
