import React from "react";
import { render, screen } from "@testing-library/react";
import { describe, it, expect, vi } from "vitest";

// Mock the components before importing Dashboard
vi.mock("../../../src/components/HeroSection", () => ({
  __esModule: true,
  default: () => <div data-testid="hero-section" />,
}));
vi.mock("../../../src/components/Roadmaps", () => ({
  __esModule: true,
  default: () => <div data-testid="roadmaps" />,
}));

import Dashboard from "../../../src/pages/dashboard";

describe("Dashboard", () => {
  it("renders without crashing", () => {
    render(<Dashboard />);
    // No error means pass
  });

  it("renders HeroSection and Roadmaps components", () => {
    render(<Dashboard />);
    expect(screen.getByTestId("hero-section")).toBeInTheDocument();
    expect(screen.getByTestId("roadmaps")).toBeInTheDocument();
  });

  it("renders a div as the root element", () => {
    const { container } = render(<Dashboard />);
    expect(container.firstChild?.nodeName).toBe("DIV");
  });
});
