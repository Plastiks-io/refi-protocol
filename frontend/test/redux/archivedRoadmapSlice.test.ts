import { describe, it, expect, vi, beforeEach } from "vitest";
import reducer, {
  fetchArchivedRoadmaps,
  resetArchivedRoadmaps,
  removeArchivedRoadmap,
  addArchivedRoadmap,
} from "../../src/redux/archivedRoadmapSlice";
import type { Roadmap } from "../../src/redux/roadmapSlice";
import axios from "axios";

vi.mock("axios");
const mockedAxios = axios as jest.Mocked<typeof axios>;

interface ArchivedRoadmap extends Roadmap {
  id: string;
  dateArchived: string;
}

const mockRoadmap: ArchivedRoadmap = {
  id: "rm-1",
  preId: "pre-abc",
  roadmapId: "rmid-123",
  roadmapName: "Test Roadmap",
  roadmapDescription: "Archived roadmap test",
  totalPlastic: 1000,
  recoveredPlastic: 200,
  createdAt: "2023-01-01",
  dateArchived: "2024-06-01",
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

describe("archivedRoadmapSlice reducer", () => {
  let initialState: ReturnType<typeof reducer>;

  beforeEach(() => {
    initialState = {
      roadmaps: [],
      loading: false,
      error: null,
    };
  });

  it("should handle addArchivedRoadmap", () => {
    const next = reducer(initialState, addArchivedRoadmap(mockRoadmap));
    expect(next.roadmaps).toHaveLength(1);
    expect(next.roadmaps[0].id).toBe("rm-1");
  });

  it("should handle removeArchivedRoadmap", () => {
    const stateWithRoadmaps = {
      ...initialState,
      roadmaps: [mockRoadmap],
    };
    const next = reducer(stateWithRoadmaps, removeArchivedRoadmap("rm-1"));
    expect(next.roadmaps).toHaveLength(0);
  });

  it("should handle resetArchivedRoadmaps", () => {
    const stateWithData = {
      roadmaps: [mockRoadmap],
      loading: true,
      error: "Some error",
    };
    const next = reducer(stateWithData, resetArchivedRoadmaps());
    expect(next.roadmaps).toEqual([]);
    expect(next.loading).toBe(false);
    expect(next.error).toBeNull();
  });

  it("should handle fetchArchivedRoadmaps.pending", () => {
    const next = reducer(initialState, {
      type: fetchArchivedRoadmaps.pending.type,
    });
    expect(next.loading).toBe(true);
    expect(next.error).toBeNull();
  });

  it("should handle fetchArchivedRoadmaps.fulfilled", () => {
    const payload = [mockRoadmap];
    const next = reducer(initialState, {
      type: fetchArchivedRoadmaps.fulfilled.type,
      payload,
    });
    expect(next.loading).toBe(false);
    expect(next.roadmaps).toEqual(payload);
  });

  it("should handle fetchArchivedRoadmaps.rejected", () => {
    const next = reducer(initialState, {
      type: fetchArchivedRoadmaps.rejected.type,
      payload: "Failed to fetch",
    });
    expect(next.loading).toBe(false);
    expect(next.error).toBe("Failed to fetch");
  });
});

describe("fetchArchivedRoadmaps asyncThunk", () => {
  beforeEach(() => {
    vi.resetAllMocks();
  });

  it("should dispatch fulfilled when axios returns roadmaps", async () => {
    mockedAxios.get.mockResolvedValueOnce({
      data: { archived_roadmaps: [mockRoadmap] },
    });

    const dispatch = vi.fn();
    const getState = vi.fn();

    const result = await fetchArchivedRoadmaps(undefined)(
      dispatch,
      getState,
      undefined
    );

    expect(result.type).toBe("roadmaps/fetchArchivedRoadmaps/fulfilled");
    expect(result.payload).toEqual([mockRoadmap]);
  });

  it("should dispatch rejected when axios fails with server message", async () => {
    mockedAxios.get.mockRejectedValueOnce({
      response: {
        data: "Server error occurred",
      },
    });

    const dispatch = vi.fn();
    const getState = vi.fn();

    const result = await fetchArchivedRoadmaps(undefined)(
      dispatch,
      getState,
      undefined
    );

    expect(result.type).toBe("roadmaps/fetchArchivedRoadmaps/rejected");
    expect(result.payload).toBe("Server error occurred");
  });

  it("should dispatch rejected when axios throws general error", async () => {
    mockedAxios.get.mockRejectedValueOnce(new Error("Network Error"));

    const dispatch = vi.fn();
    const getState = vi.fn();

    const result = await fetchArchivedRoadmaps(undefined)(
      dispatch,
      getState,
      undefined
    );

    expect(result.type).toBe("roadmaps/fetchArchivedRoadmaps/rejected");
    expect(result.payload).toBe("Error fetching roadmaps");
  });
});
