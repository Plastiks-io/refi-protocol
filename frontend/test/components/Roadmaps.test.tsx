import React from "react";
import { describe, it, expect, vi } from "vitest";
import { render, screen, fireEvent } from "@testing-library/react";
import { Provider } from "react-redux";
import { configureStore } from "@reduxjs/toolkit";
import { MemoryRouter } from "react-router-dom";
import "@testing-library/jest-dom";

import roadmapsReducer, { Roadmap } from "../../src/redux/roadmapSlice";
import completedRoadmapsReducer, {
  CompletedRoadmap,
} from "../../src/redux/completedRoadmapSlice";
import Roadmaps from "../../src/components/Roadmaps";

const mockActiveRoadmaps: Roadmap[] = [
  {
    roadmapId: "r1",
    roadmapName: "Active 1",
    roadmapDescription: "Active description",
    progress: 50,
    totalPlasticCredits: 1000,
    soldPlasticCredits: 500,
    totalPlasticTokens: 1000,
    sentPlasticTokens: 500,
    totalPlastic: 200,
    recoveredPlastic: 100,
    createdAt: "2024-01-01T00:00:00Z",
    preId: "",
    preAddress: "",
    status: "",
    fundsMissing: "",
    fundsDistributed: "",
  },
];

const mockCompletedRoadmaps: CompletedRoadmap[] = [
  {
    roadmapId: "r2",
    roadmapName: "Completed 1",
    roadmapDescription: "Completed description",
    progress: 100,
    totalPlasticCredits: 1500,
    soldPlasticCredits: 1500,
    totalPlasticTokens: 2000,
    sentPlasticTokens: 2000,
    totalPlastic: 400,
    recoveredPlastic: 400,
    createdAt: "2023-01-01T00:00:00Z",
    id: "",
    preId: "",
    preAddress: "",
    status: "",
    fundsMissing: "",
    fundsDistributed: "",
  },
];

function renderWithStore(stateOverrides = {}) {
  const store = configureStore({
    reducer: {
      roadmaps: roadmapsReducer,
      completedRoadmaps: completedRoadmapsReducer,
    },
    preloadedState: {
      roadmaps: {
        roadmaps: mockActiveRoadmaps,
        loading: false,
        error: null,
        ...((stateOverrides as { roadmaps?: any }).roadmaps || {}),
      },
      completedRoadmaps: {
        roadmaps: mockCompletedRoadmaps,
        loading: false,
        error: null,
        ...((stateOverrides as { completedRoadmaps?: any }).completedRoadmaps ||
          {}),
      },
    },
  });

  return render(
    <MemoryRouter>
      <Provider store={store}>
        <Roadmaps />
      </Provider>
    </MemoryRouter>
  );
}

describe("Roadmaps Component", () => {
  it("renders correct title based on default filter", () => {
    renderWithStore();
    expect(screen.getByText("In Progress Roadmaps")).toBeInTheDocument();
  });

  it("shows loading spinner if loading", () => {
    renderWithStore({ roadmaps: { loading: true } });
    expect(screen.getByRole("status")).toBeInTheDocument();
  });

  it("shows error message if error exists", () => {
    renderWithStore({ roadmaps: { error: "Failed to fetch" } });
    expect(
      screen.getByText("Failed to load roadmaps. Please try again later.")
    ).toBeInTheDocument();
  });

  it("renders active roadmap cards by default", () => {
    renderWithStore();
    expect(screen.getByText("Active 1")).toBeInTheDocument();
  });

  it("sorts roadmaps by newest", () => {
    renderWithStore();
    fireEvent.click(screen.getByText("Filter By"));
    fireEvent.click(screen.getByText("Newest"));
    const roadmapTitles = screen.getAllByRole("heading", { level: 3 });
    expect(roadmapTitles[0]).toHaveTextContent("Active 1");
    expect(roadmapTitles[1]).toHaveTextContent("Completed 1");
  });

  it("sorts roadmaps by oldest", () => {
    renderWithStore();
    fireEvent.click(screen.getByText("Filter By"));
    fireEvent.click(screen.getByText("Oldest"));
    const roadmapTitles = screen.getAllByRole("heading", { level: 3 });
    expect(roadmapTitles[0]).toHaveTextContent("Completed 1");
    expect(roadmapTitles[1]).toHaveTextContent("Active 1");
  });

  it("sorts roadmaps by moreKgs", () => {
    renderWithStore();
    fireEvent.click(screen.getByText("Filter By"));
    fireEvent.click(screen.getByText("More Kgs"));
    const roadmapTitles = screen.getAllByRole("heading", { level: 3 });
    expect(roadmapTitles[0]).toHaveTextContent("Completed 1");
    expect(roadmapTitles[1]).toHaveTextContent("Active 1");
  });
});
