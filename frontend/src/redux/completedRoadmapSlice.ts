import { createSlice, createAsyncThunk, PayloadAction } from "@reduxjs/toolkit";
import axios from "axios";
import { Roadmap } from "./roadmapSlice";

// extend roadmap because it has one more field called id
export interface CompletedRoadmap extends Roadmap {
  id: string;
}
// Async thunk to fetch roadmaps
export const fetchCompletedRoadmaps = createAsyncThunk<CompletedRoadmap[]>(
  "roadmaps/fetchCompletedRoadmaps",
  async (_, thunkAPI) => {
    try {
      const url = import.meta.env.VITE_SERVER_URL;
      const apiUrl = `${url}/roadmap/completed/all`;
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
const completedRoadmapSlice = createSlice({
  name: "completedRoadmaps",
  initialState: {
    roadmaps: [] as CompletedRoadmap[],
    loading: false,
    error: null as string | null,
  },
  reducers: {
    addCompletedRoadmap: (state, action: PayloadAction<CompletedRoadmap>) => {
      state.roadmaps.push(action.payload);
    },
  },
  extraReducers: (builder) => {
    builder
      .addCase(fetchCompletedRoadmaps.pending, (state) => {
        state.loading = true;
        state.error = null;
      })
      .addCase(fetchCompletedRoadmaps.fulfilled, (state, action) => {
        state.loading = false;
        state.roadmaps = action.payload;
      })
      .addCase(fetchCompletedRoadmaps.rejected, (state, action) => {
        state.loading = false;
        state.error = action.payload as string;
      });
  },
});
export const { addCompletedRoadmap } = completedRoadmapSlice.actions;

export default completedRoadmapSlice.reducer;
