import { createSlice, createAsyncThunk, PayloadAction } from "@reduxjs/toolkit";
import axios from "axios";

// Define the transaction structure
export interface Transaction {
  date: string;
  transactionFee: string;
  amount: string;
  tokenId: string;
  direction: "received" | "sent";
  pcAssetId: string;
  hash: string;
}

// Define state with pagination info
export interface TransactionState {
  transactions: Transaction[];
  loading: boolean;
  error: string | null;
  page: number;
  count: number;
  order: "asc" | "desc";
}

const initialState: TransactionState = {
  transactions: [],
  loading: false,
  error: null,
  page: 1,
  count: 5,
  order: "desc",
};

// Thunk input type
export interface FetchParams {
  address: string;
  page?: number;
  count?: number;
  order?: "asc" | "desc";
}

// Thunk to fetch transactions
export const fetchTransactions = createAsyncThunk<
  Transaction[],
  FetchParams,
  { rejectValue: string }
>("wallet/fetchTransactions", async (params, thunkAPI) => {
  try {
    const { address, page = 1, count = 5, order = "desc" } = params;
    const url = import.meta.env.VITE_SERVER_URL;
    const apiUrl = `${url}/roadmap/history/addr`;

    const response = await axios.post(
      apiUrl,
      { address }, // POST body
      {
        params: { page, count, order }, // query params
      }
    );

    return response.data.data; // Adjust based on actual response shape
  } catch (err: any) {
    return thunkAPI.rejectWithValue(
      err.response?.data?.message || "Error fetching transactions"
    );
  }
});

// Create the slice
const transactionSlice = createSlice({
  name: "transactions",
  initialState,
  reducers: {
    // Custom reset reducer
    resetTransactions: (state) => {
      state.transactions = [];
      state.loading = false;
      state.error = null;
      state.page = 1;
      state.count = 5;
      state.order = "desc";
    },
    // Optional: allow manual pagination updates
    setPagination: (
      state,
      action: PayloadAction<{
        page?: number;
        count?: number;
        order?: "asc" | "desc";
      }>
    ) => {
      if (action.payload.page !== undefined) state.page = action.payload.page;
      if (action.payload.count !== undefined)
        state.count = action.payload.count;
      if (action.payload.order !== undefined)
        state.order = action.payload.order;
    },
  },
  extraReducers: (builder) => {
    builder
      .addCase(fetchTransactions.pending, (state) => {
        state.loading = true;
        state.error = null;
      })
      .addCase(
        fetchTransactions.fulfilled,
        (state, action: PayloadAction<Transaction[]>) => {
          state.loading = false;
          state.transactions = action.payload;
        }
      )
      .addCase(fetchTransactions.rejected, (state, action) => {
        state.loading = false;
        state.error = action.payload as string;
      });
  },
});

// Export actions and reducer
export const { resetTransactions, setPagination } = transactionSlice.actions;
export default transactionSlice.reducer;
