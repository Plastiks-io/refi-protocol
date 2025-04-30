import React from "react";
import { render, screen } from "@testing-library/react";
import RoadmapCard from "../../src/components/RoadmapCard";
import { describe, it, expect } from "vitest";

const baseProps = {
  preId: "pre1",
  roadmapId: "roadmap1",
  roadmapName: "Test Roadmap",
  roadmapDescription: "This is a test roadmap.",
  progress: 75,
  totalPlasticCredits: 1000,
  soldPlasticCredits: 500,
  totalPlasticTokens: 2000,
  sentPlasticTokens: 1000,
  totalPlastic: 3000,
  recoveredPlastic: 1500,
};

describe("RoadmapCard", () => {
  it("renders roadmap name and description", () => {
    render(<RoadmapCard {...baseProps} />);
    expect(screen.getByText("Test Roadmap")).toBeInTheDocument();
    expect(screen.getByText("This is a test roadmap.")).toBeInTheDocument();
  });

  it("shows 'In Progress' badge when progress < 100", () => {
    render(<RoadmapCard {...baseProps} />);
    expect(screen.getByText("In Progress")).toBeInTheDocument();
  });

  it("show 'Completed' badge when progress is 100", () => {
    render(<RoadmapCard {...baseProps} progress={100} />);
    expect(screen.queryByText("Completed")).toBeInTheDocument();
  });

  it("renders progress percentage", () => {
    render(<RoadmapCard {...baseProps} />);
    expect(screen.getByText("75%")).toBeInTheDocument();
  });

  it("renders plastic, custody, and token details", () => {
    render(<RoadmapCard {...baseProps} />);
    expect(screen.getByText("Kg of Plastic: 1500/3000 kg")).toBeInTheDocument();
    expect(screen.getByText("Money in custody: $500/1000")).toBeInTheDocument();
    expect(screen.getByText("PLASTIK Tokens: 1000/2000")).toBeInTheDocument();
  });

  it("renders the View Details button", () => {
    render(<RoadmapCard {...baseProps} />);
    expect(
      screen.getByRole("button", { name: /view details/i })
    ).toBeInTheDocument();
  });

  it("progress bar width matches progress prop", () => {
    render(<RoadmapCard {...baseProps} progress={60} />);
    const progressBar = screen.getByRole("progressbar", { hidden: true });
    expect(progressBar).toHaveStyle({ width: "60" });
  });
});
