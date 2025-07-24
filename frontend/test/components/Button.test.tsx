import React from "react";
import { describe, it, expect, vi } from "vitest";
import { render, screen, fireEvent } from "@testing-library/react";
import "@testing-library/jest-dom";
import Button from "../../src/components/Button";
import { faCoffee } from "@fortawesome/free-solid-svg-icons";

// 1. Renders correctly with each variant
describe("Button component", () => {
  const variants = [
    "default",
    "outline",
    "dark",
    "gray",
    "userButton",
  ] as const;

  variants.forEach((variant) => {
    it(`renders correctly with variant: ${variant}`, () => {
      render(<Button variant={variant}>{variant} button</Button>);
      expect(screen.getByText(`${variant} button`)).toBeInTheDocument();
    });
  });

  // 2. Renders with an icon
  it("renders with an icon when icon prop is provided", () => {
    render(<Button icon={faCoffee}>With Icon</Button>);
    expect(screen.getByText("With Icon")).toBeInTheDocument();
    expect(document.querySelector("svg")).toBeInTheDocument();
  });

  // 3. Renders children correctly
  it("renders children (text) properly", () => {
    render(<Button>Click me</Button>);
    expect(screen.getByText("Click me")).toBeInTheDocument();
  });

  // 4. Calls onClick when clicked
  it("calls onClick handler when clicked", () => {
    const handleClick = vi.fn();
    render(<Button onClick={handleClick}>Click</Button>);
    fireEvent.click(screen.getByText("Click"));
    expect(handleClick).toHaveBeenCalledTimes(1);
  });

  // 5. Does not call onClick if disabled
  it("does not call onClick when button is disabled", () => {
    const handleClick = vi.fn();
    render(
      <Button onClick={handleClick} disabled>
        Disabled
      </Button>
    );
    fireEvent.click(screen.getByText("Disabled"));
    expect(handleClick).not.toHaveBeenCalled();
  });

  // 6. Does not render icon when icon is not passed
  it("does not render icon when icon prop is not passed", () => {
    render(<Button>No Icon</Button>);
    expect(screen.getByText("No Icon")).toBeInTheDocument();
    expect(document.querySelector("svg")).not.toBeInTheDocument();
  });
});
