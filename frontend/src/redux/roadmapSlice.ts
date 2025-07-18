import { createSlice, createAsyncThunk, PayloadAction } from "@reduxjs/toolkit";
import axios from "axios";

// Define the roadmap type (optional if using JS)
export interface Roadmap {
  preId: string;
  roadmapId: string;
  roadmapName: string;
  roadmapDescription: string;
  progress: number;
  preAddress: string;
  totalPlasticCredits: number;
  soldPlasticCredits: number;
  totalPlasticTokens: number;
  sentPlasticTokens: number;
  totalPlastic: number;
  recoveredPlastic: number;
  createdAt: string;
  status: string;
  fundsMissing: string;
  fundsDistributed: string;
}

// Async thunk to fetch roadmaps
export const fetchRoadmaps = createAsyncThunk<Roadmap[]>(
  "roadmaps/fetchRoadmaps",
  async (_, thunkAPI) => {
    try {
      const url = import.meta.env.VITE_SERVER_URL;
      const apiUrl = `${url}/roadmap/all`;
      const response = await axios.get(apiUrl); // Replace with your backend route
      return response.data.roadmaps;
    } catch (err: any) {
      return thunkAPI.rejectWithValue(
        err.response?.data || "Error fetching roadmaps"
      );
    }
  }
);

// Slice
const roadmapSlice = createSlice({
  name: "roadmaps",
  initialState: {
    roadmaps: [] as Roadmap[],
    loading: false,
    error: null as string | null,
  },
  reducers: {
    addRoadmap: (state, action: PayloadAction<Roadmap>) => {
      state.roadmaps.push(action.payload);
    },
    removeRoadmap: (state, action: PayloadAction<string>) => {
      state.roadmaps = state.roadmaps.filter(
        (r) => r.roadmapId !== action.payload
      );
    },

    updateSoldCredits: (
      state,
      action: PayloadAction<{ roadmapId: string; soldPlasticCredit: number }>
    ) => {
      const roadmap = state.roadmaps.find(
        (r) => r.roadmapId === action.payload.roadmapId
      );
      if (roadmap) {
        roadmap.soldPlasticCredits += action.payload.soldPlasticCredit;
      }
    },

    upsertRoadmap: (state, action: PayloadAction<Roadmap>) => {
      const idx = state.roadmaps.findIndex(
        (r) => r.roadmapId === action.payload.roadmapId
      );
      if (idx >= 0) {
        // replace the old roadmap object wholesale
        state.roadmaps[idx] = action.payload;
      } else {
        // new roadmap
        state.roadmaps.push(action.payload);
      }
    },
  },
  extraReducers: (builder) => {
    builder
      .addCase(fetchRoadmaps.pending, (state) => {
        state.loading = true;
        state.error = null;
      })
      .addCase(fetchRoadmaps.fulfilled, (state, action) => {
        state.loading = false;
        state.roadmaps = action.payload;
      })
      .addCase(fetchRoadmaps.rejected, (state, action) => {
        state.loading = false;
        state.error = action.payload as string;
      });
  },
});

export const { addRoadmap, removeRoadmap, updateSoldCredits, upsertRoadmap } =
  roadmapSlice.actions;
export default roadmapSlice.reducer;
