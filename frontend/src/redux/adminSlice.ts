import { createAsyncThunk, createSlice, PayloadAction } from "@reduxjs/toolkit";
import axios from "axios";

interface Admin {
  id: string;
  address: string;
  role: "SUPER_ADMIN" | "ADMIN";
  createdAt: string;
}

// Async thunk to fetch all admins
export const fetchAdmins = createAsyncThunk<Admin[]>(
  "admins/fetchAdmins",
  async (_, thunkAPI) => {
    try {
      const url = import.meta.env.VITE_SERVER_URL;
      const apiUrl = `${url}/admin/all`;
      const response = await axios.get(apiUrl, {
        withCredentials: true,
      });
      return response.data.admins;
    } catch (err: any) {
      return thunkAPI.rejectWithValue(
        err.response?.data || "Error fetching admins"
      );
    }
  }
);

export const adminSlice = createSlice({
  name: "admins",
  initialState: {
    admins: [] as Admin[],
    loading: false,
    error: null as string | null,
  },
  reducers: {
    // remove Archived roadmap
    removeAdmin: (state, action: PayloadAction<string>) => {
      state.admins = state.admins.filter((r) => r.id !== action.payload);
    },
    addAdmin: (state, action: PayloadAction<Admin>) => {
      state.admins.push(action.payload);
    },
  },
  extraReducers: (builder) => {
    builder
      .addCase(fetchAdmins.pending, (state) => {
        state.loading = true;
        state.error = null;
      })
      .addCase(fetchAdmins.fulfilled, (state, action) => {
        state.loading = false;
        state.admins = action.payload;
      })
      .addCase(fetchAdmins.rejected, (state, action) => {
        state.loading = false;
        state.error = action.payload as string;
      });
  },
});

export const { removeAdmin, addAdmin } = adminSlice.actions;
export default adminSlice.reducer;
