import React from "react";
import { describe, it, expect, vi } from "vitest";
import { render, screen } from "@testing-library/react";
import TokenBalance from "../../../src/components/community/TokenBalance";
import { faCircleCheck } from "@fortawesome/free-solid-svg-icons";

// Mock Button component
vi.mock("../../../src/components/Button", () => ({
  default: ({ children, className, icon }: any) => (
    <button data-testid="button" className={className} data-icon={!!icon}>
      {children}
    </button>
  ),
}));

describe("TokenBalance", () => {
  it("renders the token balance formatted", () => {
    render(<TokenBalance plastikBalance={12345} votingAllowed={true} />);
    expect(screen.getByText("12,345 Plastik")).toBeInTheDocument();
  });

  it("shows eligible message and correct styles when votingAllowed is true", () => {
    render(<TokenBalance plastikBalance={100} votingAllowed={true} />);
    const button = screen.getByTestId("button");
    expect(button).toHaveTextContent("Eligible to Start Voting Round");
    expect(button).toHaveClass("bg-gray-900");
    expect(button).toHaveClass("text-white");
    expect(button.getAttribute("data-icon")).toBe("true");
  });

  it("shows not eligible message and correct styles when votingAllowed is false", () => {
    render(<TokenBalance plastikBalance={100} votingAllowed={false} />);
    const button = screen.getByTestId("button");
    expect(button).toHaveTextContent("Not Eligible to Start Voting Round");
    expect(button).toHaveClass("bg-gray-300");
    expect(button).toHaveClass("text-black");
    expect(button.getAttribute("data-icon")).toBe("false");
  });

  it("renders the heading", () => {
    render(<TokenBalance plastikBalance={0} votingAllowed={false} />);
    expect(screen.getByText("Your Token Balance")).toBeInTheDocument();
  });

  it("formats large numbers correctly", () => {
    render(<TokenBalance plastikBalance={1000000} votingAllowed={false} />);
    expect(screen.getByText("1,000,000 Plastik")).toBeInTheDocument();
  });
});
