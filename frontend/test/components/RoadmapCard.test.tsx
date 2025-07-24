import React from "react";
import { describe, it, expect, beforeEach, vi } from "vitest";
import { render, screen, fireEvent } from "@testing-library/react";
import "@testing-library/jest-dom";
import { useNavigate } from "react-router-dom";
import RoadmapCard from "../../src/components/RoadmapCard";
import type { Roadmap } from "../../src/redux/roadmapSlice";

vi.mock("react-router-dom", async () => {
  const actual = await vi.importActual("react-router-dom");
  return {
    ...actual,
    useNavigate: vi.fn(),
  };
});

describe("RoadmapCard Component", () => {
  const mockNavigate = vi.fn();

  beforeEach(() => {
    vi.clearAllMocks();
    (useNavigate as jest.Mock).mockReturnValue(mockNavigate);
  });

  const sampleRoadmap: Roadmap = {
    roadmapId: "rmp-123",
    roadmapName: "Test Roadmap",
    roadmapDescription: "This is a test description.",
    progress: 80,
    totalPlasticCredits: 1000,
    soldPlasticCredits: 800,
    totalPlasticTokens: 5000,
    sentPlasticTokens: 2000,
    totalPlastic: 100,
    recoveredPlastic: 75,
    preId: "",
    preAddress: "",
    createdAt: "",
    status: "",
    fundsMissing: "",
    fundsDistributed: "",
  };

  it("renders all roadmap details correctly", () => {
    render(<RoadmapCard {...sampleRoadmap} />);

    expect(screen.getByText("Test Roadmap")).toBeInTheDocument();
    expect(screen.getByText("This is a test description.")).toBeInTheDocument();
    expect(screen.getByText("In Progress")).toBeInTheDocument();
    expect(screen.getByText("Progress")).toBeInTheDocument();
    expect(screen.getByText("80%")).toBeInTheDocument();
    expect(screen.getByText("Kg of Plastic: 75/100 kg")).toBeInTheDocument();
    expect(screen.getByText("Money in custody: $800/1000")).toBeInTheDocument();
    expect(
      screen.getByText("PLASTIK Tokens Transacted: 2000/5000")
    ).toBeInTheDocument();
    expect(screen.getByText("View Details")).toBeInTheDocument();
  });

  it("shows 'Completed' when progress is 100", () => {
    render(<RoadmapCard {...sampleRoadmap} progress={100} />);
    expect(screen.getByText("Completed")).toBeInTheDocument();
  });

  it("navigates to the correct route on button click", () => {
    render(<RoadmapCard {...sampleRoadmap} />);
    fireEvent.click(screen.getByText("View Details"));
    expect(mockNavigate).toHaveBeenCalledWith("/roadmap/rmp-123");
  });

  it("has a progressbar element", () => {
    render(<RoadmapCard {...sampleRoadmap} />);
    expect(screen.getByRole("progressbar")).toBeInTheDocument();
  });

  it("handles undefined or null values gracefully", () => {
    const brokenProps = {
      ...sampleRoadmap,
      recoveredPlastic: null as any,
      soldPlasticCredits: undefined as any,
      sentPlasticTokens: NaN,
    };

    render(<RoadmapCard {...brokenProps} />);
    expect(screen.getByText(/Kg of Plastic:/)).toBeInTheDocument();
    expect(screen.getByText(/Money in custody:/)).toBeInTheDocument();
    expect(screen.getByText(/PLASTIK Tokens Transacted:/)).toBeInTheDocument();
  });
});
