import React from "react";
import { describe, it, expect } from "vitest";
import { render, screen } from "@testing-library/react";
import StatsCard from "../../src/components/StatsCard";
import "@testing-library/jest-dom";
describe("StatsCard Component", () => {
  it("renders title, value, description, and icon", () => {
    render(
      <StatsCard
        title="Total Revenue"
        value="$10,000"
        description="Compared to last month"
        icon="/icons/revenue.svg"
        width="1/3"
      />
    );

    expect(screen.getByText("Total Revenue")).toBeInTheDocument();
    expect(screen.getByText("$10,000")).toBeInTheDocument();
    expect(screen.getByText("Compared to last month")).toBeInTheDocument();
    expect(screen.getByRole("img")).toHaveAttribute(
      "src",
      "/icons/revenue.svg"
    );
  });

  it("does not render description if not provided", () => {
    render(
      <StatsCard
        title="New Users"
        value="1,200"
        icon="/icons/users.svg"
        width="1/4"
      />
    );

    expect(screen.getByText("New Users")).toBeInTheDocument();
    expect(screen.getByText("1,200")).toBeInTheDocument();
    expect(screen.queryByText(/Compared to/i)).not.toBeInTheDocument();
  });

  it("applies correct width class based on width prop", () => {
    const { container } = render(
      <StatsCard
        title="Carbon Saved"
        value="32kg"
        description="Eco-friendly"
        icon="/icons/carbon.svg"
        width="2/5"
      />
    );

    const card = container.firstChild as HTMLElement;
    expect(card.className).toContain("lg:w-2/5");
  });

  it("supports all valid width options", () => {
    const widths = ["1/4", "1/3", "2/5", "12/25"] as const;

    widths.forEach((width) => {
      const { container, unmount } = render(
        <StatsCard
          title="Demo"
          value="Test"
          icon="/icons/demo.svg"
          width={width}
        />
      );

      const card = container.firstChild as HTMLElement;
      expect(card.className).toContain(`lg:w-${width}`);
      unmount();
    });
  });
});
