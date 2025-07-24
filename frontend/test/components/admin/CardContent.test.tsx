import { describe, it, expect } from "vitest";
import { render, screen } from "@testing-library/react";
import React from "react";
import CardContent from "../../../src/components/admin/CardContent";
import "@testing-library/jest-dom"; // Import jest-dom matchers
describe("CardContent", () => {
  it("renders title and value correctly", () => {
    render(<CardContent title="Total Users" value={1500} />);
    expect(screen.getByText("Total Users")).toBeInTheDocument();
    expect(screen.getByText("1500")).toBeInTheDocument();
  });

  it("renders subtitle when provided", () => {
    render(<CardContent title="Revenue" value="$20K" subtitle="This Month" />);
    expect(screen.getByText("This Month")).toBeInTheDocument();
  });

  it("does not render subtitle when not provided", () => {
    render(<CardContent title="Users" value="250" />);
    const subtitle = screen.queryByText(
      (content, node) => !!node?.classList.contains("text-gray-400")
    );
    expect(subtitle).not.toBeInTheDocument();
  });

  it("renders progress bar with correct width", () => {
    render(<CardContent title="Progress" value="70%" progress={70} />);
    const progressBar = screen.getByRole("presentation");
    expect(progressBar).toHaveStyle("width: 70%");
  });

  it("does not render progress bar when progress is undefined", () => {
    render(<CardContent title="Tasks" value="5" />);
    const progressBar = screen.queryByRole("presentation");
    expect(progressBar).not.toBeInTheDocument();
  });

  it("supports value as a string", () => {
    render(<CardContent title="Status" value="Complete" />);
    expect(screen.getByText("Complete")).toBeInTheDocument();
  });

  it("supports value as a number", () => {
    render(<CardContent title="Score" value={95} />);
    expect(screen.getByText("95")).toBeInTheDocument();
  });
});
