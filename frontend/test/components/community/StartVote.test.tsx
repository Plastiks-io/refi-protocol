import React from "react";
import { describe, it, expect, vi, beforeEach, Mock } from "vitest";
import { render, screen, fireEvent } from "@testing-library/react";
import StartVote from "../../../src/components/community/StartVote";
import axios from "axios";

vi.mock("axios"); // Mock entire axios module
(axios.post as Mock).mockResolvedValue({ data: "Success" });

vi.mock("../../../src/components/Button", () => ({
  default: ({ children, ...props }: any) => (
    <button {...props}>{children}</button>
  ),
}));

const defaultProps = {
  eligibleforVoting: true,
  oldRetirementRate: 2,
  newRetirementRate: 3,
  oldRetirementPercentage: 60,
  newRetirementPercentage: 40,
};

describe("StartVote", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("renders the title and description", () => {
    render(<StartVote {...defaultProps} />);
    expect(screen.getByText("Token Retirement Rate Vote")).toBeInTheDocument();
    expect(
      screen.getByText(
        /Cast your vote to determine the percentage of tokens to be retired/i
      )
    ).toBeInTheDocument();
  });

  it("shows eligibility status", () => {
    render(<StartVote {...defaultProps} />);
    expect(screen.getByText("Eligible to Vote")).toBeInTheDocument();
  });

  it("shows not eligible status if not eligible", () => {
    render(<StartVote {...defaultProps} eligibleforVoting={false} />);
    expect(screen.getByText("Not Eligible to Vote")).toBeInTheDocument();
  });

  it("renders both vote options with correct rates", () => {
    render(<StartVote {...defaultProps} />);
    expect(
      screen.getByText("Maintain 2% Token Retirement")
    ).toBeInTheDocument();
    expect(
      screen.getByText("Increase to 3% Token Retirement")
    ).toBeInTheDocument();
  });

  it("selects the old option when clicked", () => {
    render(<StartVote {...defaultProps} />);
    const oldOption = screen.getByText("Maintain 2% Token Retirement");
    fireEvent.click(oldOption);
    const radio = screen.getAllByRole("radio")[0] as HTMLInputElement;
    expect(radio.checked).toBe(true);
  });

  it("selects the new option when clicked", () => {
    render(<StartVote {...defaultProps} />);
    const newOption = screen.getByText("Increase to 3% Token Retirement");
    fireEvent.click(newOption);
    const radio = screen.getAllByRole("radio")[1] as HTMLInputElement;
    expect(radio.checked).toBe(true);
  });

  it("calls alert with correct value when casting vote for old option", () => {
    window.alert = vi.fn();
    render(<StartVote {...defaultProps} />);
    fireEvent.click(screen.getByText("Maintain 2% Token Retirement"));
    fireEvent.click(screen.getByText("Cast Your Vote"));
    expect(window.alert).toHaveBeenCalledWith(2);
  });

  it("calls alert with correct value when casting vote for new option", () => {
    window.alert = vi.fn();
    render(<StartVote {...defaultProps} />);
    fireEvent.click(screen.getByText("Increase to 3% Token Retirement"));
    fireEvent.click(screen.getByText("Cast Your Vote"));
    expect(window.alert).toHaveBeenCalledWith(3);
  });

  it("shows the correct vote counts", () => {
    render(<StartVote {...defaultProps} />);
    expect(screen.getByText("89,425 votes")).toBeInTheDocument();
    expect(screen.getByText("67,317 votes")).toBeInTheDocument();
  });

  it("shows the eligibility info message", () => {
    render(<StartVote {...defaultProps} />);
    expect(
      screen.getByText(
        "You must hold at least 1000 PLASTIK tokens to be eligible to vote."
      )
    ).toBeInTheDocument();
  });

  it("renders progress bars with correct widths", () => {
    render(<StartVote {...defaultProps} />);
    const bars = screen.getAllByRole("progressbar");
    // Since role is not set, fallback to querySelector
    const progressDivs = document.querySelectorAll(
      ".bg-black.h-2.rounded-full"
    );
    expect(progressDivs[0]).toHaveStyle(
      `width: ${defaultProps.oldRetirementPercentage}%`
    );
    expect(progressDivs[1]).toHaveStyle(
      `width: ${defaultProps.newRetirementPercentage}%`
    );
  });
});
