import { describe, it, vi, beforeEach, Mock } from "vitest";
import { render, screen } from "@testing-library/react";
import React from "react";
import { useSelector } from "react-redux";
import Admin from "../../../src/pages/dashboard/admin";
import axios from "axios";

vi.mock("axios"); // Mock entire axios module
(axios.post as Mock).mockResolvedValue({ data: "Success" });

// Mock RoadmapRow component
vi.mock("../../../src/components/RoadmapRow", () => ({
  __esModule: true,
  default: (props: any) => (
    <tr data-testid="roadmap-row">
      <td>{JSON.stringify(props)}</td>
    </tr>
  ),
}));


// Mock lucide-react icons
vi.mock("lucide-react", () => ({
  Loader: (props: any) => <svg data-testid="loader" {...props} />,
  AlertCircle: (props: any) => <svg data-testid="alert-circle" {...props} />,
}));

vi.mock("react-redux", async () => {
  const actual = await vi.importActual<typeof import("react-redux")>(
    "react-redux"
  );
  return {
    ...actual,
    useSelector: vi.fn(),
  };
});

const mockUseSelector = useSelector as unknown as Mock;

describe("Admin Dashboard Page", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("renders loading state", () => {
    mockUseSelector.mockImplementation((cb) =>
      cb({ roadmaps: { roadmaps: [], loading: true, error: null } })
    );
    render(<Admin />);
    expect(screen.getByTestId("loader")).toBeInTheDocument();
    expect(screen.getByText(/Loading roadmaps/i)).toBeInTheDocument();
  });

  it("renders error state", () => {
    mockUseSelector.mockImplementation((cb) =>
      cb({ roadmaps: { roadmaps: [], loading: false, error: "Some error" } })
    );
    render(<Admin />);
    expect(screen.getByTestId("alert-circle")).toBeInTheDocument();
    expect(screen.getByText(/Failed to load roadmaps/i)).toBeInTheDocument();
  });

  it("renders empty state", () => {
    mockUseSelector.mockImplementation((cb) =>
      cb({ roadmaps: { roadmaps: [], loading: false, error: null } })
    );
    render(<Admin />);
    expect(screen.getByTestId("alert-circle")).toBeInTheDocument();
    expect(screen.getByText(/No roadmaps available/i)).toBeInTheDocument();
  });

it("renders roadmaps table with rows", () => {
    const mockRoadmaps = [
        { id: 1, name: "Roadmap 1", entity: "Entity 1" },
        { id: 2, name: "Roadmap 2", entity: "Entity 2" },
    ];
    mockUseSelector.mockImplementation((cb) =>
        cb({ roadmaps: { roadmaps: mockRoadmaps, loading: false, error: null } })
    );
    render(<Admin />);
    // Use getAllByRole to avoid "Found multiple elements" error
    const tables = screen.getAllByRole("table");
    // The main table is the first one
    expect(tables[0]).toBeInTheDocument();
    expect(screen.getAllByTestId("roadmap-row")).toHaveLength(2);
});

  it("renders Transaction History section", () => {
    mockUseSelector.mockImplementation((cb) =>
      cb({ roadmaps: { roadmaps: [], loading: false, error: null } })
    );
    render(<Admin />);
    expect(screen.getByText(/Transaction History/i)).toBeInTheDocument();
    expect(
      screen.getByText(/Below is a record of recent transactions/i)
    ).toBeInTheDocument();
  });
});
