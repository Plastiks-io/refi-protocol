// src/store/transactionSlice.ts

import { createSlice, createAsyncThunk, PayloadAction } from "@reduxjs/toolkit";
import axios from "axios";

// Enum for transaction type
export enum TransactionType {
  Sold = "creditSale",
  Roadmap = "roadmapCompletion",
  Token = "tokenReturn",
  USDM = "usdmReleased",
  Transfer = "fundTransfer",
}

// Interface for a single transaction record
export interface Transaction {
  id: string;
  txDate: Date;
  txFee: number;
  amount: number;
  roadmapId: string;
  assetId: string;
  hash: string;
  type: TransactionType;
  createdAt: Date;
}

// Pagination metadata returned by the backend
interface PaginationMeta {
  total: number;
  page: number;
  perPage: number;
  totalPages: number;
}

// Shape of the API response
interface FetchTransactionsResponse {
  transactions: Transaction[];
  meta: PaginationMeta;
}

// Slice state shape
interface TransactionState {
  transactions: Transaction[];
  loading: boolean;
  error: string | null;
  page: number;
  perPage: number;
  total: number;
  totalPages: number;
}

const initialState: TransactionState = {
  transactions: [],
  loading: false,
  error: null,
  page: 1,
  perPage: 20,
  total: 0,
  totalPages: 1,
};

// Async thunk to fetch a page of transactions
export const fetchTransactions = createAsyncThunk<
  FetchTransactionsResponse,
  { page?: number; perPage?: number }
>("transactions/fetchAll", async ({ page = 1, perPage = 20 }) => {
  const baseUrl = import.meta.env.VITE_SERVER_URL;
  const response = await axios.get<FetchTransactionsResponse>(
    `${baseUrl}/transaction/all`,
    { params: { page, perPage } }
  );
  return response.data;
});

const transactionSlice = createSlice({
  name: "transactions",
  initialState,
  reducers: {
    // You can add non-async reducers here if needed
  },
  extraReducers: (builder) => {
    builder
      .addCase(fetchTransactions.pending, (state) => {
        state.loading = true;
        state.error = null;
      })
      .addCase(
        fetchTransactions.fulfilled,
        (state, action: PayloadAction<FetchTransactionsResponse>) => {
          state.transactions = action.payload.transactions;
          state.page = action.payload.meta.page;
          state.perPage = action.payload.meta.perPage;
          state.total = action.payload.meta.total;
          state.totalPages = action.payload.meta.totalPages;
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
