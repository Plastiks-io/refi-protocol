import React from "react";
import { describe, it, expect } from "vitest";
import { render, screen } from "@testing-library/react";
import "@testing-library/jest-dom";

import Footer from "../../src/components/Footer";

describe("Footer component", () => {
  it("renders the footer without crashing", () => {
    render(<Footer />);
    expect(
      screen.getByText("© 2025 Plastiks DApp. All rights reserved.")
    ).toBeInTheDocument();
  });

  it("displays all social media icons", () => {
    render(<Footer />);
    expect(screen.getByTestId("icon-twitter")).toBeInTheDocument();
    expect(screen.getByTestId("icon-discord")).toBeInTheDocument();
    expect(screen.getByTestId("icon-github")).toBeInTheDocument();
  });

  it("social icons have valid href attributes", () => {
    render(<Footer />);
    const twitterLink = screen.getByTestId("icon-twitter");
    const discordLink = screen.getByTestId("icon-discord");
    const githubLink = screen.getByTestId("icon-github");

    expect(twitterLink).toHaveAttribute("href", "#");
    expect(discordLink).toHaveAttribute("href", "#");
    expect(githubLink).toHaveAttribute("href", "#");
  });

  it("social icons have expected class styles", () => {
    render(<Footer />);
    const icons = [
      screen.getByTestId("icon-twitter"),
      screen.getByTestId("icon-discord"),
      screen.getByTestId("icon-github"),
    ];

    icons.forEach((icon) => {
      expect(icon).toHaveClass("text-white");
      expect(icon).toHaveClass("hover:text-gray-400");
    });
  });
});
