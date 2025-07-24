import React from "react";
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen, fireEvent } from "@testing-library/react";
import "@testing-library/jest-dom";
import { useNavigate } from "react-router-dom";
import RoadmapRow from "../../src/components/RoadmapRow";
import type { Roadmap } from "../../src/redux/roadmapSlice";

vi.mock("react-router-dom", async () => {
  const actual = await vi.importActual("react-router-dom");
  return {
    ...actual,
    useNavigate: vi.fn(),
  };
});

describe("RoadmapRow Component", () => {
  const mockNavigate = vi.fn();

  beforeEach(() => {
    vi.clearAllMocks();
    (useNavigate as jest.Mock).mockReturnValue(mockNavigate);
  });

  const baseProps: Roadmap = {
    preId: "PRE-001",
    roadmapId: "roadmap-123",
    roadmapName: "Test Roadmap",
    roadmapDescription: "",
    progress: 75,
    totalPlastic: 0,
    recoveredPlastic: 0,
    totalPlasticCredits: 1200,
    soldPlasticCredits: 0,
    totalPlasticTokens: 3400,
    sentPlasticTokens: 0,
    fundsMissing: "100",
    preAddress: "",
    createdAt: "",
    status: "",
    fundsDistributed: "",
  };

  it("renders roadmap info correctly", () => {
    render(
      <table>
        <tbody>
          <RoadmapRow {...baseProps} />
        </tbody>
      </table>
    );

    expect(screen.getByText("PRE-001")).toBeInTheDocument();
    expect(screen.getByText("Test Roadmap")).toBeInTheDocument();
    expect(screen.getByText("1,200 USDM")).toBeInTheDocument();
    expect(screen.getByText("3,400 PLASTIK")).toBeInTheDocument();
  });

  it("shows 'Pending' status when fundsMissing > 0", () => {
    render(
      <table>
        <tbody>
          <RoadmapRow {...baseProps} />
        </tbody>
      </table>
    );

    expect(screen.getByText("Pending")).toBeInTheDocument();
  });

  it("shows 'Up to date' status when fundsMissing === 0", () => {
    render(
      <table>
        <tbody>
          <RoadmapRow {...baseProps} fundsMissing={0} />
        </tbody>
      </table>
    );

    expect(screen.getByText("Up to date")).toBeInTheDocument();
  });

  it("calls navigate with correct route on button click", () => {
    render(
      <table>
        <tbody>
          <RoadmapRow {...baseProps} />
        </tbody>
      </table>
    );

    fireEvent.click(screen.getByText("View Details"));
    expect(mockNavigate).toHaveBeenCalledWith("/roadmap/roadmap-123");
  });
});
