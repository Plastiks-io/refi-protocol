import { describe, it, expect, vi, beforeEach } from "vitest";
import reducer, {
  fetchAdmins,
  addAdmin,
  removeAdmin,
} from "../../src/redux/adminSlice";
import { Admin } from "../../src/redux/adminSlice";
import axios from "axios";

vi.mock("axios");
const mockedAxios = axios as jest.Mocked<typeof axios>;

const mockAdmin: Admin = {
  id: "admin-1",
  address: "addr_test1q...",
  role: "ADMIN",
  createdAt: "2024-01-01T00:00:00Z",
};

describe("adminSlice reducer", () => {
  let initialState: ReturnType<typeof reducer>;

  beforeEach(() => {
    initialState = {
      admins: [],
      loading: false,
      error: null,
    };
  });

  it("should handle addAdmin", () => {
    const nextState = reducer(initialState, addAdmin(mockAdmin));
    expect(nextState.admins).toHaveLength(1);
    expect(nextState.admins[0].id).toBe("admin-1");
  });

  it("should handle removeAdmin", () => {
    const stateWithAdmin = {
      ...initialState,
      admins: [mockAdmin],
    };
    const nextState = reducer(stateWithAdmin, removeAdmin("admin-1"));
    expect(nextState.admins).toHaveLength(0);
  });

  it("should handle fetchAdmins.pending", () => {
    const nextState = reducer(initialState, { type: fetchAdmins.pending.type });
    expect(nextState.loading).toBe(true);
    expect(nextState.error).toBeNull();
  });

  it("should handle fetchAdmins.fulfilled", () => {
    const admins = [mockAdmin];
    const nextState = reducer(initialState, {
      type: fetchAdmins.fulfilled.type,
      payload: admins,
    });
    expect(nextState.loading).toBe(false);
    expect(nextState.admins).toEqual(admins);
  });

  it("should handle fetchAdmins.rejected", () => {
    const nextState = reducer(initialState, {
      type: fetchAdmins.rejected.type,
      payload: "Failed to fetch",
    });
    expect(nextState.loading).toBe(false);
    expect(nextState.error).toBe("Failed to fetch");
  });
});

describe("fetchAdmins asyncThunk", () => {
  beforeEach(() => {
    vi.resetAllMocks();
  });

  it("should dispatch fulfilled when axios returns admins", async () => {
    mockedAxios.get.mockResolvedValueOnce({
      data: { admins: [mockAdmin] },
    });

    const thunkAPI = {
      rejectWithValue: vi.fn(),
      fulfillWithValue: vi.fn((v) => ({
        type: "admins/fetchAdmins/fulfilled",
        payload: v,
      })),
    } as any;

    const dispatch = vi.fn();
    const getState = vi.fn();
    const resultAction = await fetchAdmins()(dispatch, getState, undefined);
    expect(resultAction.type).toBe("admins/fetchAdmins/fulfilled");
    expect(resultAction.payload).toEqual([mockAdmin]);
  });

  it("should dispatch rejected when axios fails with server message", async () => {
    mockedAxios.get.mockRejectedValueOnce({
      response: {
        data: "Server error occurred",
      },
    });

    const thunkAPI = {
      rejectWithValue: vi.fn((v) => ({
        type: "admins/fetchAdmins/rejected",
        payload: v,
      })),
    } as any;

    const result = await fetchAdmins()(vi.fn(), vi.fn(), undefined);
    expect(result.type).toBe("admins/fetchAdmins/rejected");
    expect(result.payload).toBe("Server error occurred");
  });

  it("should dispatch rejected when axios throws general error", async () => {
    mockedAxios.get.mockRejectedValueOnce(new Error("Network Error"));

    const result = await fetchAdmins()(vi.fn(), vi.fn(), undefined);
    expect(result.type).toBe("admins/fetchAdmins/rejected");
    expect(result.payload).toBe("Error fetching admins");
  });
});
