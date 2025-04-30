import React from "react";
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen, fireEvent } from "@testing-library/react";
import { Provider } from "react-redux";
import { MemoryRouter } from "react-router-dom";
import Navbar from "../../src/components/Navbar";
import { store } from "../../src/redux/store";

// Mock meshsdk methods
vi.mock("@meshsdk/core", () => ({
  BrowserWallet: {
    getAvailableWallets: vi.fn(() =>
      Promise.resolve([
        { name: "Nami", id: "nami" },
        { name: "Eternl", id: "eternl" },
      ])
    ),
    enable: vi.fn(() =>
      Promise.resolve({
        getChangeAddress: () => Promise.resolve("addr_test1q..."),
      })
    ),
  },
}));

// Mock Sonner toast
vi.mock("sonner", () => ({
  toast: {
    error: vi.fn(),
  },
}));

describe("Navbar Component", () => {
  beforeEach(() => {
    render(
      <Provider store={store}>
        <MemoryRouter>
          <Navbar />
        </MemoryRouter>
      </Provider>
    );
  });

  it("renders the logo and title", () => {
    expect(screen.getByAltText("Plastiks")).toBeInTheDocument();
    expect(screen.getByText("Plastiks")).toBeInTheDocument();
    expect(screen.getByText("ReFi Dapp")).toBeInTheDocument();
  });

  it("renders navigation links", () => {
    expect(screen.getByText("Roadmaps")).toBeInTheDocument();
    expect(screen.getByText("Transactions")).toBeInTheDocument();
    expect(screen.getByText("Community")).toBeInTheDocument();
    expect(screen.getByText("Buy NFT")).toBeInTheDocument();
  });

  it("renders connect wallet button if no wallet connected", () => {
    expect(screen.getByText("Connect Wallet")).toBeInTheDocument();
  });

  it("shows popup on clicking Connect Wallet", async () => {
    const connectButton = screen.getByText("Connect Wallet");
    fireEvent.click(connectButton);
    // Wallet selection popup should now be open
    expect(await screen.findByText("Nami")).toBeInTheDocument();
    expect(await screen.findByText("Eternl")).toBeInTheDocument();
  });
});
