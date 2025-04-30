import React from "react";
import { describe, it, expect, vi } from "vitest";
import { render, screen, fireEvent } from "@testing-library/react";
import VotingPopup from "../../../src/components/community/VotingPopup";

describe("VotingPopup", () => {
  const defaultProps = {
    votingPercentage: 30,
    votingDuration: 7,
    onStartVoting: vi.fn(),
    onClose: vi.fn(),
  };

  it("renders the popup with correct title", () => {
    render(<VotingPopup {...defaultProps} />);
    expect(screen.getByText("Create New Voting Round")).toBeInTheDocument();
  });

  it("displays the correct voting percentage", () => {
    render(<VotingPopup {...defaultProps} />);
    expect(screen.getByText("30%")).toBeInTheDocument();
  });

  it("displays the correct voting duration and end date", () => {
    render(<VotingPopup {...defaultProps} />);
    expect(
      screen.getByText(/This voting round will last for 7 days until/)
    ).toBeInTheDocument();
  });

  it("calls onClose when the close icon is clicked", () => {
    render(<VotingPopup {...defaultProps} />);
    const closeButton = screen.getAllByRole("button")[0];
    fireEvent.click(closeButton);
    expect(defaultProps.onClose).toHaveBeenCalled();
  });

  it("calls onClose when the Cancel button is clicked", () => {
    render(<VotingPopup {...defaultProps} />);
    const cancelButton = screen.getByText("Cancel");
    fireEvent.click(cancelButton);
    expect(defaultProps.onClose).toHaveBeenCalled();
  });

  it("calls onStartVoting when the Create Voting Round button is clicked", () => {
    render(<VotingPopup {...defaultProps} />);
    const createButton = screen.getByText("Create Voting Round");
    fireEvent.click(createButton);
    expect(defaultProps.onStartVoting).toHaveBeenCalled();
  });

  it("renders with different votingPercentage and votingDuration", () => {
    render(
      <VotingPopup
        votingPercentage={55}
        votingDuration={14}
        onStartVoting={vi.fn()}
        onClose={vi.fn()}
      />
    );
    expect(screen.getByText("55%")).toBeInTheDocument();
    expect(
      screen.getByText(/This voting round will last for 14 days until/)
    ).toBeInTheDocument();
  });
});
