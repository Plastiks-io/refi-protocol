import { createSlice, createAsyncThunk, PayloadAction } from "@reduxjs/toolkit";
import axios from "axios";

export enum TransactionType {
  Sold = "creditSale",
  Transfer = "fundTransfer",
}

export interface Transaction {
  id: string;
  txDate: string;
  txFee: number;
  amount: number;
  assetId: string;
  hash: string;
  type: TransactionType;
  createdAt: string;
}

interface TransactionState {
  transactions: Transaction[];
  loading: boolean;
  error: string | null;
}

const initialState: TransactionState = {
  transactions: [],
  loading: false,
  error: null,
};

// Thunks
export const fetchTransactions = createAsyncThunk(
  "transactions/fetchAll",
  async () => {
    const url = import.meta.env.VITE_SERVER_URL;
    const apiUrl = `${url}/transaction/all`;
    const response = await axios.get(apiUrl);
    return response.data.transactions;
  }
);

// Slice
const transactionSlice = createSlice({
  name: "transactions",
  initialState,
  reducers: {},
  extraReducers: (builder) => {
    builder
      // Fetch
      .addCase(fetchTransactions.pending, (state) => {
        state.loading = true;
        state.error = null;
      })
      .addCase(
        fetchTransactions.fulfilled,
        (state, action: PayloadAction<Transaction[]>) => {
          state.transactions = action.payload;
          state.loading = false;
        }
      )
      .addCase(fetchTransactions.rejected, (state, action) => {
        state.loading = false;
        state.error = action.error.message || "Failed to fetch transactions";
      });
  },
});

export default transactionSlice.reducer;
