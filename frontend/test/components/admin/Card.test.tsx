import { render, screen } from "@testing-library/react";
import Card from "../../../src/components/admin/Card";
import React from "react";

describe("Card component", () => {
  it("renders children correctly", () => {
    render(
      <Card>
        <span>Test Content</span>
      </Card>
    );
    expect(screen.getByText("Test Content")).toBeInTheDocument();
  });

  it("applies default classes", () => {
    render(<Card>Content</Card>);
    const div = screen.getByText("Content");
    expect(div).toHaveClass("bg-white");
    expect(div).toHaveClass("rounded-2xl");
    expect(div).toHaveClass("shadow-md");
    expect(div).toHaveClass("p-4");
  });

  it("appends custom className", () => {
    render(<Card className="custom-class">Content</Card>);
    const div = screen.getByText("Content");
    expect(div).toHaveClass("custom-class");
  });

  it("renders multiple children", () => {
    render(
      <Card>
        <span>Child 1</span>
        <span>Child 2</span>
      </Card>
    );
    expect(screen.getByText("Child 1")).toBeInTheDocument();
    expect(screen.getByText("Child 2")).toBeInTheDocument();
  });

  it("renders without crashing when no className is provided", () => {
    render(<Card>Content</Card>);
    expect(screen.getByText("Content")).toBeInTheDocument();
  });
});
