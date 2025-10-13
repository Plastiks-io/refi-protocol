// src/redux/governanceSlice.ts
import { createAsyncThunk, createSlice, PayloadAction } from "@reduxjs/toolkit";
// Adjust this import path if your store is elsewhere
import type { RootState, AppDispatch } from "./store";

//
// Types
//
export interface ActiveProposal {
  proposalId: string; // bigints serialized as strings
  retirementPercentage: string;
  proposerPkh: string;
  createdAt: string; // timestamp string (ms) — convert to BigInt or Number on client if needed
  expiresAt: string;
  votesFor: string;
  votesAgainst: string;
  executed: boolean;
}

export interface GovernanceStats {
  currentRetirementRate: string;
  roundDuration: string;
  totalVotes: string;
  totalPlastikVoted: string;
  hasActiveProposal: boolean;
  activeProposal?: ActiveProposal | null;
}

export interface GovernanceState {
  data: GovernanceStats | null;
  loading: boolean;
  error: string | null;
}

const initialState: GovernanceState = {
  data: null,
  loading: false,
  error: null,
};

//
// Async thunk to fetch governance stats
//
export const fetchGovernanceStats = createAsyncThunk<
  GovernanceStats, // return type on success
  void, // arg type
  {
    dispatch: AppDispatch;
    state: RootState;
    rejectValue: string;
  }
>("governance/fetchStats", async (_arg, { rejectWithValue }) => {
  try {
    const base = import.meta.env.VITE_SERVER_URL ?? "";
    const url = `${base.replace(/\/$/, "")}/governance/stats`; // ensure no trailing slash duplication

    const res = await fetch(url, {
      method: "GET",
      credentials: "include", // include cookies if API uses them
      headers: {
        "Content-Type": "application/json",
      },
    });

    if (!res.ok) {
      const txt = await res.text().catch(() => res.statusText);
      return rejectWithValue(`Server responded with ${res.status}: ${txt}`);
    }

    const json = (await res.json()) as GovernanceStats;

    // Basic validation/normalization: ensure fields exist and in expected shape
    const safe: GovernanceStats = {
      currentRetirementRate: json.currentRetirementRate ?? "0%",
      roundDuration: json.roundDuration ?? "No active voting round",
      totalVotes: json.totalVotes ?? "0",
      totalPlastikVoted: json.totalPlastikVoted ?? "0",
      hasActiveProposal: Boolean(json.hasActiveProposal),
      activeProposal: json.activeProposal ?? null,
    };

    return safe;
  } catch (err: any) {
    return rejectWithValue(
      err?.message ?? "Network error fetching governance stats"
    );
  }
});

//
// Slice
//
const governanceSlice = createSlice({
  name: "governance",
  initialState,
  reducers: {
    // optional: clear/reset
    clearGovernanceState(state) {
      state.data = null;
      state.loading = false;
      state.error = null;
    },
    // optional: update a cached active proposal locally
    updateActiveProposal(state, action: PayloadAction<ActiveProposal | null>) {
      if (!state.data) {
        state.data = {
          currentRetirementRate: "0%",
          roundDuration: "No active voting round",
          totalVotes: "0",
          totalPlastikVoted: "0",
          hasActiveProposal: !!action.payload,
          activeProposal: action.payload,
        };
      } else {
        state.data.hasActiveProposal = !!action.payload;
        state.data.activeProposal = action.payload;
      }
    },
  },
  extraReducers: (builder) => {
    builder
      .addCase(fetchGovernanceStats.pending, (state) => {
        state.loading = true;
        state.error = null;
      })
      .addCase(
        fetchGovernanceStats.fulfilled,
        (state, action: PayloadAction<GovernanceStats>) => {
          state.loading = false;
          state.error = null;
          state.data = action.payload;
        }
      )
      .addCase(fetchGovernanceStats.rejected, (state, action) => {
        state.loading = false;
        state.error =
          action.payload ??
          action.error.message ??
          "Failed to fetch governance stats";
      });
  },
});

export const { clearGovernanceState, updateActiveProposal } =
  governanceSlice.actions;

export default governanceSlice.reducer;
