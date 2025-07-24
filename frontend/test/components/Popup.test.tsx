import React from "react";
import { describe, it, expect, vi } from "vitest";
import { render, screen, fireEvent } from "@testing-library/react";
import Popup from "../../src/components/Popup";
import "@testing-library/jest-dom"; // ✅ Required for toBeInTheDocument()

const mockWallets = [
  {
    id: "nami",
    name: "Nami Wallet",
    version: "1.0.0",
    icon: "https://example.com/nami.png",
  },
  {
    id: "eternl",
    name: "Eternl Wallet",
    version: "1.2.3",
    icon: "https://example.com/eternl.png",
  },
];

describe("Popup Component", () => {
  it("renders wallet options correctly", () => {
    render(
      <Popup
        wallets={mockWallets}
        onSelectWallet={() => {}}
        onClose={() => {}}
      />
    );

    expect(screen.getByText("Connect Wallet")).toBeInTheDocument();
    expect(screen.getByText("Nami Wallet")).toBeInTheDocument();
    expect(screen.getByText("Eternl Wallet")).toBeInTheDocument();

    const images = screen.getAllByRole("img");
    expect(images).toHaveLength(2);
    expect(images[0]).toHaveAttribute("src", "https://example.com/nami.png");
  });

  it("calls onSelectWallet with correct ID on wallet click", () => {
    const onSelectMock = vi.fn();

    render(
      <Popup
        wallets={mockWallets}
        onSelectWallet={onSelectMock}
        onClose={() => {}}
      />
    );

    fireEvent.click(screen.getByText("Nami Wallet"));
    expect(onSelectMock).toHaveBeenCalledWith("nami");

    fireEvent.click(screen.getByText("Eternl Wallet"));
    expect(onSelectMock).toHaveBeenCalledWith("eternl");
  });
});
