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

export const { addRoadmap, removeRoadmap } = roadmapSlice.actions;
export default roadmapSlice.reducer;
