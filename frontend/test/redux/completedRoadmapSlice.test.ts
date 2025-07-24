import { describe, it, expect, vi, beforeEach } from "vitest";
import reducer, {
  fetchCompletedRoadmaps,
  addCompletedRoadmap,
} from "../../src/redux/completedRoadmapSlice";
import type { CompletedRoadmap } from "../../src/redux/completedRoadmapSlice";
import axios from "axios";

vi.mock("axios");
const mockedAxios = axios as jest.Mocked<typeof axios>;

const mockRoadmap: CompletedRoadmap = {
  id: "rm-1",
  preId: "pre-abc",
  roadmapId: "rmid-123",
  roadmapName: "Completed Roadmap",
  roadmapDescription: "Testing completed roadmap slice",
  totalPlastic: 1000,
  recoveredPlastic: 300,
  createdAt: "2023-12-01",
  progress: 0,
  preAddress: "",
  totalPlasticCredits: 0,
  soldPlasticCredits: 0,
  totalPlasticTokens: 0,
  sentPlasticTokens: 0,
  status: "",
  fundsMissing: "",
  fundsDistributed: "",
};

describe("completedRoadmapSlice reducer", () => {
  let initialState: ReturnType<typeof reducer>;

  beforeEach(() => {
    initialState = {
      roadmaps: [],
      loading: false,
      error: null,
    };
  });

  it("should handle addCompletedRoadmap", () => {
    const next = reducer(initialState, addCompletedRoadmap(mockRoadmap));
    expect(next.roadmaps).toHaveLength(1);
    expect(next.roadmaps[0].id).toBe("rm-1");
  });

  it("should handle fetchCompletedRoadmaps.pending", () => {
    const next = reducer(initialState, {
      type: fetchCompletedRoadmaps.pending.type,
    });
    expect(next.loading).toBe(true);
    expect(next.error).toBeNull();
  });

  it("should handle fetchCompletedRoadmaps.fulfilled", () => {
    const payload = [mockRoadmap];
    const next = reducer(initialState, {
      type: fetchCompletedRoadmaps.fulfilled.type,
      payload,
    });
    expect(next.loading).toBe(false);
    expect(next.roadmaps).toEqual(payload);
  });

  it("should handle fetchCompletedRoadmaps.rejected", () => {
    const next = reducer(initialState, {
      type: fetchCompletedRoadmaps.rejected.type,
      payload: "Failed to fetch",
    });
    expect(next.loading).toBe(false);
    expect(next.error).toBe("Failed to fetch");
  });
});

describe("fetchCompletedRoadmaps asyncThunk", () => {
  beforeEach(() => {
    vi.resetAllMocks();
  });

  it("should dispatch fulfilled when axios returns roadmaps", async () => {
    mockedAxios.get.mockResolvedValueOnce({
      data: { roadmaps: [mockRoadmap] },
    });

    const dispatch = vi.fn();
    const getState = vi.fn();

    const result = await fetchCompletedRoadmaps(undefined)(
      dispatch,
      getState,
      undefined
    );

    expect(dispatch).toHaveBeenCalled();
    expect(result.payload).toEqual([mockRoadmap]);
    expect(result.type).toBe("roadmaps/fetchCompletedRoadmaps/fulfilled");
  });

  it("should dispatch rejected when axios fails with server message", async () => {
    mockedAxios.get.mockRejectedValueOnce({
      response: {
        data: "Server error occurred",
      },
    });

    const dispatch = vi.fn();
    const getState = vi.fn();

    const result = await fetchCompletedRoadmaps(undefined)(
      dispatch,
      getState,
      undefined
    );

    expect(dispatch).toHaveBeenCalled();
    expect(result.payload).toBe("Server error occurred");
    expect(result.type).toBe("roadmaps/fetchCompletedRoadmaps/rejected");
  });

  it("should dispatch rejected when axios throws general error", async () => {
    mockedAxios.get.mockRejectedValueOnce(new Error("Network Error"));

    const dispatch = vi.fn();
    const getState = vi.fn();

    const result = await fetchCompletedRoadmaps(undefined)(
      dispatch,
      getState,
      undefined
    );

    expect(dispatch).toHaveBeenCalled();
    expect(result.payload).toBe("Error fetching roadmaps");
    expect(result.type).toBe("roadmaps/fetchCompletedRoadmaps/rejected");
  });
});
