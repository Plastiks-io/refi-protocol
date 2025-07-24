import React from "react";
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen, fireEvent } from "@testing-library/react";
import "@testing-library/jest-dom";
import HeroSection from "../../src/components/HeroSection";

vi.mock("react-redux", async () => {
  const actual = await vi.importActual("react-redux");
  return {
    ...actual,
    useSelector: vi.fn(),
  };
});

import { useSelector } from "react-redux";

const mockActiveRoadmaps = [
  { recoveredPlastic: 100, soldPlasticCredits: 10 },
  { recoveredPlastic: 200, soldPlasticCredits: 20 },
];

const mockCompletedRoadmaps = [
  { recoveredPlastic: 300, soldPlasticCredits: 30 },
  { recoveredPlastic: 400, soldPlasticCredits: 40 },
];

describe("HeroSection Component", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    Element.prototype.scrollIntoView = vi.fn(); // ✅ Fix scrollIntoView issue
  });

  it("renders stats cards when data is available", () => {
    (useSelector as any).mockImplementation((selectorFn: any) => {
      return selectorFn({
        roadmaps: {
          roadmaps: mockActiveRoadmaps,
          loading: false,
          error: null,
        },
        completedRoadmaps: {
          roadmaps: mockCompletedRoadmaps,
          loading: false,
          error: null,
        },
      });
    });

    render(<HeroSection />);

    expect(screen.getByText("Active Roadmaps")).toBeInTheDocument();
    expect(screen.getByText("Total Completed Roadmaps")).toBeInTheDocument();
    expect(screen.getByText("Plastic Recovered")).toBeInTheDocument();
    expect(screen.getByText("1,000 kg")).toBeInTheDocument();
    expect(screen.getByText("Funds in Escrow Wallet")).toBeInTheDocument();
    expect(screen.getByText("30")).toBeInTheDocument();
    expect(screen.getByText("Total Funding Distributed")).toBeInTheDocument();
    expect(screen.getByText("70")).toBeInTheDocument();
    expect(screen.getByText("Total Tokens Retired")).toBeInTheDocument();
    expect(screen.getByText("140")).toBeInTheDocument();
  });

  it("shows error when error is present", () => {
    (useSelector as any).mockImplementation((selectorFn: any) => {
      return selectorFn({
        roadmaps: {
          roadmaps: [],
          loading: false,
          error: "Something went wrong",
        },
        completedRoadmaps: {
          roadmaps: [],
          loading: false,
          error: null,
        },
      });
    });

    render(<HeroSection />);
    expect(
      screen.getByText("Failed to load stats. Please try again later.")
    ).toBeInTheDocument();
  });

  it("scrolls to bottom when 'Roadmaps' button is clicked", () => {
    (useSelector as any).mockImplementation((selectorFn: any) => {
      return selectorFn({
        roadmaps: {
          roadmaps: [],
          loading: false,
          error: null,
        },
        completedRoadmaps: {
          roadmaps: [],
          loading: false,
          error: null,
        },
      });
    });

    render(<HeroSection />);
    const btn = screen.getByText("Roadmaps");
    fireEvent.click(btn);
    expect(Element.prototype.scrollIntoView).toHaveBeenCalled(); // ✅ Now succeeds
  });
});
