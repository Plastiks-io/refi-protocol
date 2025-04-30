import React from "react";
import { render, screen, fireEvent, waitFor } from "@testing-library/react";
import { Provider } from "react-redux";
import { MemoryRouter } from "react-router-dom";
import { configureStore } from "@reduxjs/toolkit";
import Roadmaps from "../../src/components/Roadmaps";
import roadmapsReducer from "../../src/redux/roadmapSlice";
import completedRoadmapReducer from "../../src/redux/completedRoadmapSlice";
import { Roadmap } from "../../src/redux/roadmapSlice";

// Mock initial state
const mockInitialState = {
  roadmaps: {
    roadmaps: [
      {
        preId: "pre1",
        roadmapId: "roadmap1",
        roadmapName: "Roadmap Name",
        roadmapDescription: "Roadmap description",
        progress: 50,
        adminPkh: "adminPkh1",
        prePkh: "prePkh1",
        preSkh: "preSkh1",
        totalPlasticCredits: 100,
        soldPlasticCredits: 50,
        totalPlasticTokens: 10000,
        sentPlasticTokens: 5000,
        totalPlastic: 100,
        recoveredPlastic: 50,
      },
    ],
    loading: false,
    error: null as string | null,
  },
  completedRoadmaps: {
    roadmaps: [],
    loading: false,
    error: null as string | null,
  },
};

const renderWithProviders = (
  ui,
  {
    preloadedState = {
      roadmaps: mockInitialState.roadmaps,
      completedRoadmaps: mockInitialState.completedRoadmaps,
    },
  } = {}
) => {
  const store = configureStore({
    reducer: {
      roadmaps: roadmapsReducer,
      completedRoadmaps: completedRoadmapReducer,
    },
    preloadedState,
  });

  return render(
    <Provider store={store}>
      <MemoryRouter>{ui}</MemoryRouter>
    </Provider>
  );
};

describe("Roadmaps component", () => {
  it("renders active roadmaps by default", () => {
    renderWithProviders(<Roadmaps />);
    expect(screen.getByText("Active Roadmaps")).toBeInTheDocument();
    expect(
      screen.getByText(/Active roadmaps are currently in progress/i)
    ).toBeInTheDocument();
    expect(screen.getByTestId("roadmap-card")).toHaveTextContent(
      "Roadmap Name"
    );
  });

  it("shows loading spinner when loading", () => {
    const loadingState = {
      roadmaps: {
        loading: true,
        error: null,
        roadmaps: [],
      },
      completedRoadmaps: {
        roadmaps: [],
        loading: false,
        error: null,
      },
    };
    renderWithProviders(<Roadmaps />, {
      preloadedState: loadingState,
    });
    expect(screen.getByRole("status")).toBeInTheDocument();
  });

  it("shows specific error message if error exists", () => {
    const errorState = {
      roadmaps: {
        loading: false,
        error: "An unexpected error occurred",
        roadmaps: [],
      },
      completedRoadmaps: {
        roadmaps: [],
        loading: false,
        error: null,
      },
    };
    renderWithProviders(<Roadmaps />, { preloadedState: errorState });

    // Directly test for the specific error message
    expect(
      screen.getByText("Error: An unexpected error occurred")
    ).toBeInTheDocument();
  });

  it("shows 'No matching roadmaps found.' if filteredData is empty", () => {
    const emptyState = {
      roadmaps: {
        loading: false,
        error: null,
        roadmaps: [],
      },
      completedRoadmaps: {
        roadmaps: [],
        loading: false,
        error: null,
      },
    };
    renderWithProviders(<Roadmaps />, { preloadedState: emptyState });
    expect(screen.getByText("No matching roadmaps found.")).toBeInTheDocument();
  });

  it("opens and closes filters panel", () => {
    renderWithProviders(<Roadmaps />);
    fireEvent.click(screen.getByRole("button", { name: "Filters" }));
    expect(screen.getByText("Search Roadmaps")).toBeInTheDocument();
    fireEvent.click(screen.getByRole("button", { name: /X/i }));
    expect(screen.queryByText("Search Roadmaps")).not.toBeInTheDocument();
  });

  it("filters roadmaps by search term", async () => {
    renderWithProviders(<Roadmaps />);
    fireEvent.click(screen.getByRole("button", { name: "Filters" }));
    const input = screen.getByPlaceholderText("Search by name...");
    fireEvent.change(input, { target: { value: "Roadmap" } });

    await waitFor(() => {
      expect(screen.getByTestId("roadmap-card")).toHaveTextContent("Roadmap");
      expect(screen.queryByText("Alpha")).not.toBeInTheDocument();
    });
  });
});
