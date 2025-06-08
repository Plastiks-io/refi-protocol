import { createSlice, createAsyncThunk, PayloadAction } from "@reduxjs/toolkit";
import axios from "axios";
import { Roadmap } from "./roadmapSlice";

// Define the roadmap type (optional if using JS)
interface ArchivedRoadmap extends Roadmap {
  id: string;
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
    addArchivedRoadmap: (state, action: PayloadAction<ArchivedRoadmap>) => {
      state.roadmaps.push(action.payload);
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

export const { resetArchivedRoadmaps, removeArchivedRoadmap, addArchivedRoadmap } =
  archivedRoadmapSlice.actions;
export default archivedRoadmapSlice.reducer;
