import { createSlice, createAsyncThunk, PayloadAction } from "@reduxjs/toolkit";
import axios from "axios";

// Define the roadmap type (optional if using JS)
export interface ArchivedRoadmap {
  id: string;
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
  dateArchived: string;
}

// Async thunk to fetch roadmaps
export const fetchArchivedRoadmaps = createAsyncThunk<ArchivedRoadmap[]>(
  "roadmaps/fetchArchivedRoadmaps",
  async (_, thunkAPI) => {
    try {
      const url = import.meta.env.VITE_SERVER_URL;
      const apiUrl = `${url}/roadmap/archived/all`;
      const response = await axios.get(apiUrl); // Replace with your backend route
      return response.data.archived_roadmaps;
    } catch (err: any) {
      return thunkAPI.rejectWithValue(
        err.response?.data || "Error fetching roadmaps"
      );
    }
  }
);

// Slice
const archivedRoadmapSlice = createSlice({
  name: "archivedRoadmaps",
  initialState: {
    roadmaps: [] as ArchivedRoadmap[],
    loading: false,
    error: null as string | null,
  },
  reducers: {
    // Custom reset reducer
    resetArchivedRoadmaps: (state) => {
      state.roadmaps = [];
      state.loading = false;
      state.error = null;
    },
    // remove Archived roadmap
    removeArchivedRoadmap: (state, action: PayloadAction<string>) => {
      state.roadmaps = state.roadmaps.filter((r) => r.id !== action.payload);
    },
  },
  extraReducers: (builder) => {
    builder
      .addCase(fetchArchivedRoadmaps.pending, (state) => {
        state.loading = true;
        state.error = null;
      })
      .addCase(fetchArchivedRoadmaps.fulfilled, (state, action) => {
        state.loading = false;
        state.roadmaps = action.payload;
      })
      .addCase(fetchArchivedRoadmaps.rejected, (state, action) => {
        state.loading = false;
        state.error = action.payload as string;
      });
  },
});

export const { resetArchivedRoadmaps, removeArchivedRoadmap } =
archivedRoadmapSlice.actions;
export default archivedRoadmapSlice.reducer;
