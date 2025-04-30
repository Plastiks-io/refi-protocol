import React from "react";
import { render, screen } from "@testing-library/react";
import HeroSection from "../../src/components/HeroSection";
import { faDatabase, faFire, faLeaf } from "@fortawesome/free-solid-svg-icons";

const stats = [
  {
    title: "Total Tokens Retired",
    value: "234,567",
    description: "+2.5% from last month",
    icon: faFire,
  },
  {
    title: "Stablecoins Available",
    value: "$892,450",
    description: "USDC + USDM",
    icon: faDatabase,
  },
  {
    title: "Plastic Recovered",
    value: "45,678 kg",
    description: "Equivalent to x amount of bottles",
    icon: faLeaf,
  },
];
describe("HeroSection", () => {
  beforeEach(() => {
    render(<HeroSection />);
  });
  it("renders the main heading", () => {
    expect(
      screen.getByRole("heading", {
        name: /building a sustainable future with refi/i,
      })
    ).toBeInTheDocument();
  });

  it("renders the description paragraph", () => {
    expect(
      screen.getByText(
        /Plastiks ReFi Protocol ensures transparent and traceable funding/i
      )
    ).toBeInTheDocument();
  });

  it("renders the CTA button", () => {
    expect(screen.getByRole("button", { name: /cta/i })).toBeInTheDocument();
  });

  it("renders all stats cards with correct titles, values and descriptions", () => {
    stats.forEach(({ title, value, description }) => {
      expect(screen.getByText(title)).toBeInTheDocument();
      expect(screen.getByText(value)).toBeInTheDocument();
      expect(screen.getByText(description)).toBeInTheDocument();
    });
  });
});
