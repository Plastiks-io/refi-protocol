import React from "react";
import { render, screen } from "@testing-library/react";
import CardContent from "../../../src/components/admin/CardContent";

describe("CardContent", () => {
  it("renders title and value", () => {
    render(<CardContent title="Test Title" value="123" />);
    expect(screen.getByText("Test Title")).toBeInTheDocument();
    expect(screen.getByText("123")).toBeInTheDocument();
  });

  it("renders subtitle when provided", () => {
    render(<CardContent title="Title" value="Value" subtitle="Subtitle" />);
    expect(screen.getByText("Subtitle")).toBeInTheDocument();
  });

  it("does not render subtitle when not provided", () => {
    render(<CardContent title="Title" value="Value" />);
    expect(screen.queryByText("Subtitle")).not.toBeInTheDocument();
  });

  it("renders progress bar when progress is provided", () => {
    render(<CardContent title="Title" value="Value" progress={75} />);
    const progressBar = screen.getByRole("presentation", { hidden: true });
    expect(progressBar).toBeInTheDocument();
    expect(progressBar).toHaveStyle({ width: "75%" });
  });

  it("does not render progress bar when progress is not provided", () => {
    const { container } = render(<CardContent title="Title" value="Value" />);
    const progressBar = container.querySelector(".bg-black.h-2.rounded");
    expect(progressBar).not.toBeInTheDocument();
  });

  it("renders numeric value", () => {
    render(<CardContent title="Title" value={42} />);
    expect(screen.getByText("42")).toBeInTheDocument();
  });

  it("applies correct classes", () => {
    render(<CardContent title="Title" value="Value" />);
    expect(screen.getByText("Title")).toHaveClass(
      "text-gray-600",
      "text-sm",
      "font-medium",
      "mb-1"
    );
    expect(screen.getByText("Value")).toHaveClass(
      "text-xl",
      "font-bold",
      "text-black"
    );
  });
});
