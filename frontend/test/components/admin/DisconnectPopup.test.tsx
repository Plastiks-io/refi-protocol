import { describe, it, expect, vi } from "vitest";
import { render, screen, fireEvent } from "@testing-library/react";
import React from "react";
import DisconnectPopup from "../../../src/components/admin/DisconnectPopup";
import "@testing-library/jest-dom"; // Import jest-dom matchers
describe("DisconnectPopup", () => {
  const mockOnConfirm = vi.fn();
  const mockOnCancel = vi.fn();

  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("does not render when isOpen is false", () => {
    render(
      <DisconnectPopup
        isOpen={false}
        onConfirm={mockOnConfirm}
        onCancel={mockOnCancel}
      />
    );

    // nothing in the popup should be visible
    expect(screen.queryByText("Disconnect Wallet")).not.toBeInTheDocument();
  });

  it("renders when isOpen is true", () => {
    render(
      <DisconnectPopup
        isOpen={true}
        onConfirm={mockOnConfirm}
        onCancel={mockOnCancel}
      />
    );

    expect(screen.getByText("Disconnect Wallet")).toBeInTheDocument();
    expect(
      screen.getByText("Are you sure you want to disconnect your wallet?")
    ).toBeInTheDocument();
    expect(screen.getByText("Cancel")).toBeInTheDocument();
    expect(screen.getByText("Disconnect")).toBeInTheDocument();
  });

  it("calls onCancel when Cancel button is clicked", () => {
    render(
      <DisconnectPopup
        isOpen={true}
        onConfirm={mockOnConfirm}
        onCancel={mockOnCancel}
      />
    );

    const cancelButton = screen.getByText("Cancel");
    fireEvent.click(cancelButton);

    expect(mockOnCancel).toHaveBeenCalledTimes(1);
  });

  it("calls onConfirm when Disconnect button is clicked", () => {
    render(
      <DisconnectPopup
        isOpen={true}
        onConfirm={mockOnConfirm}
        onCancel={mockOnCancel}
      />
    );

    const disconnectButton = screen.getByText("Disconnect");
    fireEvent.click(disconnectButton);

    expect(mockOnConfirm).toHaveBeenCalledTimes(1);
  });

  it("has proper classes for responsiveness and accessibility", () => {
    render(
      <DisconnectPopup
        isOpen={true}
        onConfirm={mockOnConfirm}
        onCancel={mockOnCancel}
      />
    );

    const dialog = screen.getByText("Disconnect Wallet").closest("div");
    expect(dialog?.className).toContain("bg-white");
    expect(dialog?.className).toContain("rounded-2xl");
  });
});
