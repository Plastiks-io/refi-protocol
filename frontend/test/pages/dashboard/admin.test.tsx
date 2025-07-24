import { describe, it, vi, beforeEach, expect } from "vitest";
import { render, screen, fireEvent, waitFor } from "@testing-library/react";
import React from "react";
import "@testing-library/jest-dom";

// ✅ Define createMockUseSelector before all mocks (to avoid hoisting issue)
const createMockUseSelector =
  (customState?: Partial<any>) => (selector: any) => {
    const baseState = {
      roadmaps: {
        roadmaps: [
          {
            roadmapId: "1",
            roadmapName: "Active Roadmap",
            totalPlastic: 20,
            createdAt: new Date("2023-01-01").toISOString(),
          },
        ],
        loading: false,
        error: null,
      },
      completedRoadmaps: {
        roadmaps: [
          {
            roadmapId: "2",
            roadmapName: "Completed Roadmap",
            totalPlastic: 50,
            createdAt: new Date("2022-01-01").toISOString(),
          },
        ],
        loading: false,
        error: null,
      },
      transactions: {
        transactions: [],
        page: 1,
        perPage: 10,
        totalPages: 2,
      },
    };
    return selector({ ...baseState, ...customState });
  };

// ✅ Dynamic mock setup function to inject state
const mockReactRedux = (customState?: Partial<any>) => {
  vi.doMock("react-redux", async () => {
    const actual = await vi.importActual("react-redux");
    return {
      ...actual,
      useDispatch: () => vi.fn(),
      useSelector: createMockUseSelector(customState),
    };
  });
};

// 🔁 Static mocks
vi.mock("react-paginate", () => ({
  __esModule: true,
  default: ({ onPageChange }: any) => (
    <button onClick={() => onPageChange({ selected: 1 })}>
      Mock Pagination
    </button>
  ),
}));

vi.mock("../../../src/components/TransactionList", () => ({
  __esModule: true,
  default: () => <div data-testid="transaction-list">TransactionList</div>,
}));

vi.mock("../../../src/components/RoadmapRow", () => ({
  __esModule: true,
  default: (props: any) => (
    <tr data-testid="roadmap-row">
      <td>{props.roadmapName}</td>
    </tr>
  ),
}));

vi.mock("../../../src/components/CheckboxFilterList", () => ({
  __esModule: true,
  default: ({ onChange }: any) => (
    <div data-testid="filter-list">
      <button onClick={() => onChange(["completed"])}>Completed Filter</button>
    </div>
  ),
}));

vi.mock("../../../src/redux/TransactionSlice", () => ({
  fetchTransactions: vi.fn(),
  TransactionType: {
    Sold: "sold",
    Transfer: "transfer",
  },
}));

describe("AdminPage", () => {
  beforeEach(() => {
    vi.resetModules(); // Required to reset mocks between tests
    vi.clearAllMocks();
    mockReactRedux(); // default mock setup
  });

  it("renders heading and initial content", async () => {
    const { default: AdminPage } = await import(
      "../../../src/pages/dashboard/admin"
    );
    render(<AdminPage />);
    expect(screen.getByText("Active Roadmaps")).toBeInTheDocument();
    expect(
      screen.getByText(/Below is a list of active roadmaps/i)
    ).toBeInTheDocument();
  });

  it("renders roadmap rows", async () => {
    const { default: AdminPage } = await import(
      "../../../src/pages/dashboard/admin"
    );
    render(<AdminPage />);
    expect(screen.getByTestId("roadmap-row")).toBeInTheDocument();
    expect(screen.getByText("Active Roadmap")).toBeInTheDocument();
  });

  it("shows the transaction list", async () => {
    const { default: AdminPage } = await import(
      "../../../src/pages/dashboard/admin"
    );
    render(<AdminPage />);
    expect(screen.getByTestId("transaction-list")).toBeInTheDocument();
  });

  it("displays filter dropdown and updates filter on selection", async () => {
    const { default: AdminPage } = await import(
      "../../../src/pages/dashboard/admin"
    );
    render(<AdminPage />);
    fireEvent.click(screen.getByText("Filter By"));
    fireEvent.click(screen.getByText("Completed Filter"));
    await waitFor(() => {
      expect(screen.getByText("Completed Roadmap")).toBeInTheDocument();
    });
  });

  it("handles pagination click", async () => {
    const { default: AdminPage } = await import(
      "../../../src/pages/dashboard/admin"
    );
    render(<AdminPage />);
    fireEvent.click(screen.getByText("Mock Pagination"));
    expect(screen.getByText("Mock Pagination")).toBeInTheDocument();
  });

  it("renders loading state", async () => {
    mockReactRedux({
      roadmaps: { roadmaps: [], loading: true, error: null },
    });
    const { default: AdminPage } = await import(
      "../../../src/pages/dashboard/admin"
    );
    render(<AdminPage />);
    expect(screen.getByText("Loading roadmaps...")).toBeInTheDocument();
  });

  it("renders error state", async () => {
    mockReactRedux({
      roadmaps: { roadmaps: [], loading: false, error: "Error loading" },
    });
    const { default: AdminPage } = await import(
      "../../../src/pages/dashboard/admin"
    );
    render(<AdminPage />);
    expect(
      screen.getByText("Failed to load roadmaps. Please try again.")
    ).toBeInTheDocument();
  });

  it("renders empty state", async () => {
    mockReactRedux({
      roadmaps: { roadmaps: [], loading: false, error: null },
      completedRoadmaps: { roadmaps: [], loading: false, error: null },
    });
    const { default: AdminPage } = await import(
      "../../../src/pages/dashboard/admin"
    );
    render(<AdminPage />);
    expect(screen.getByText("No roadmaps available.")).toBeInTheDocument();
  });
});
