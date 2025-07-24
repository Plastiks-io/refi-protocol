// test/redux/transactionSlice.test.ts
import { describe, it, expect, vi, beforeEach } from "vitest";
import reducer, {
  fetchTransactions,
  TransactionType,
} from "../../src/redux/TransactionSlice";
import axios from "axios";

vi.mock("axios");
const mockedAxios = axios as jest.Mocked<typeof axios>;

const mockTransaction = {
  id: "tx-1",
  txDate: new Date("2024-01-01"),
  txFee: 1.5,
  amount: 100,
  roadmapId: "rm-123",
  assetId: "asset-xyz",
  hash: "hash-123",
  type: TransactionType.Sold,
  createdAt: new Date("2024-01-01"),
};

const mockResponse = {
  data: {
    transactions: [mockTransaction],
    meta: {
      total: 1,
      page: 1,
      perPage: 10,
      totalPages: 1,
    },
  },
};

describe("transactionSlice reducer", () => {
  let initialState: ReturnType<typeof reducer>;

  beforeEach(() => {
    initialState = {
      transactions: [],
      loading: false,
      error: null,
      page: 1,
      perPage: 10,
      total: 0,
      totalPages: 1,
      currentFilter: [TransactionType.Sold],
    };
  });

  it("should handle fetchTransactions.pending", () => {
    const next = reducer(initialState, {
      type: fetchTransactions.pending.type,
    });
    expect(next.loading).toBe(true);
    expect(next.error).toBeNull();
  });

  it("should handle fetchTransactions.fulfilled", () => {
    const next = reducer(initialState, {
      type: fetchTransactions.fulfilled.type,
      payload: mockResponse.data,
    });
    expect(next.loading).toBe(false);
    expect(next.transactions).toEqual([mockTransaction]);
    expect(next.page).toBe(1);
    expect(next.total).toBe(1);
  });

  it("should handle fetchTransactions.rejected", () => {
    const next = reducer(initialState, {
      type: fetchTransactions.rejected.type,
      error: { message: "API failed" },
    });
    expect(next.loading).toBe(false);
    expect(next.error).toBe("API failed");
  });
});

describe("fetchTransactions async thunk", () => {
  beforeEach(() => {
    vi.resetAllMocks();
  });

  it("should fetch transactions successfully", async () => {
    mockedAxios.get.mockResolvedValueOnce(mockResponse);

    const dispatch = vi.fn();
    const getState = vi.fn();

    const thunk = fetchTransactions({
      page: 1,
      perPage: 10,
      type: TransactionType.Sold,
    });
    const result = await thunk(dispatch, getState, undefined);

    expect(mockedAxios.get).toHaveBeenCalled();
    expect(result.payload).toEqual(mockResponse.data);
    expect(result.type).toBe("transactions/fetchAll/fulfilled");
  });

  it("should handle API failure and dispatch rejected", async () => {
    mockedAxios.get.mockRejectedValueOnce(new Error("Network Error"));

    const dispatch = vi.fn();
    const getState = vi.fn();

    const result = await fetchTransactions({
      page: 1,
      perPage: 10,
      type: TransactionType.Token,
    })(dispatch, getState, undefined);

    expect(result.type).toBe("transactions/fetchAll/rejected");
  });

  it("should append multiple types in query params", async () => {
    mockedAxios.get.mockResolvedValueOnce(mockResponse);
    const dispatch = vi.fn();
    const getState = vi.fn();

    const result = await fetchTransactions({
      type: [TransactionType.Sold, TransactionType.USDM],
    })(dispatch, getState, undefined);

    expect(mockedAxios.get).toHaveBeenCalledWith(
      expect.stringContaining("type=creditSale&type=usdmReleased")
    );
    expect(result.type).toBe("transactions/fetchAll/fulfilled");
  });

  it("should include roadmapId in request if provided", async () => {
    mockedAxios.get.mockResolvedValueOnce(mockResponse);
    const dispatch = vi.fn();
    const getState = vi.fn();

    await fetchTransactions({
      page: 2,
      perPage: 20,
      type: TransactionType.Roadmap,
      roadmapId: "rm-456",
    })(dispatch, getState, undefined);

    expect(mockedAxios.get).toHaveBeenCalledWith(
      expect.stringContaining("roadmapId=rm-456")
    );
  });
});
