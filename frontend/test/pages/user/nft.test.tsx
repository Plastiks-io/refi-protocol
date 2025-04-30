import React from "react";
import { render, screen, waitFor, fireEvent } from "@testing-library/react";
import { Provider } from "react-redux";
import { configureStore } from "@reduxjs/toolkit";
import { describe, beforeEach, test, expect, vi } from "vitest";
import NFTPurchase from "../../../src/pages/user/nft";
import { WalletContext } from "../../../src/App";
import { toast } from "sonner";
import roadmapSlice from "../../../src/redux/roadmapSlice";
import { BrowserWallet } from "@meshsdk/core";

// Mock external dependencies
vi.mock("sonner");
vi.mock("../../../src/services/BuyNFT");
vi.mock("lucide-react", () => ({
  Loader2: vi.fn(() => <div data-testid="spinner">Loading...</div>),
}));

const mockStore = configureStore({
  reducer: {
    roadmaps: roadmapSlice,
  },
  preloadedState: {
    roadmaps: {
      roadmaps: [],
      loading: false,
      error: null,
    },
  },
});

// Mock BrowserWallet implementation
const mockWallet = {
  getChangeAddress: vi.fn().mockResolvedValue("mock-address"),
  getBalance: vi.fn(),
  getUsedAddresses: vi.fn(),
  getRewardAddresses: vi.fn(),
  getUnusedAddresses: vi.fn(),
  signTx: vi.fn(),
  submitTx: vi.fn(),
  // Add other required BrowserWallet methods as needed
} as unknown as BrowserWallet;

const renderComponent = (wallet: BrowserWallet | null = null) => {
  return render(
    <WalletContext.Provider value={wallet}>
      <Provider store={mockStore}>
        <NFTPurchase />
      </Provider>
    </WalletContext.Provider>
  );
};

describe("NFTPurchase Component", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    mockStore.dispatch = vi.fn();
  });

  it("1. Renders loading spinner when data is loading", () => {
    vi.spyOn(mockStore, "getState").mockReturnValue({
      roadmaps: { roadmaps: [], loading: true, error: null },
    });

    renderComponent();
    expect(screen.getByTestId("spinner")).toBeInTheDocument();
  });

  it("2. Displays error message when there is an error", () => {
    vi.spyOn(mockStore, "getState").mockReturnValue({
      roadmaps: { roadmaps: [], loading: false, error: "Test error" },
    });

    renderComponent();
    expect(screen.getByText(/Error: Test error/)).toBeInTheDocument();
  });

  it("3. Displays roadmap data when loaded", async () => {
    const mockRoadmaps = [
      {
        preId: "pre-1",
        roadmapId: "roadmap-1",
        roadmapName: "Test Roadmap",
        roadmapDescription: "Test Description",
        progress: 0,
        adminPkh: "admin-pkh",
        prePkh: "pre-pkh",
        preSkh: "pre-skh",
        totalPlasticCredits: 100,
        soldPlasticCredits: 50,
        totalPlasticTokens: 0,
        sentPlasticTokens: 0,
        totalPlastic: 100,
        recoveredPlastic: 0,
      },
    ];

    vi.spyOn(mockStore, "getState").mockReturnValue({
      roadmaps: { roadmaps: mockRoadmaps, loading: false, error: null },
    });

    renderComponent();
    expect(screen.getByText("Test Roadmap")).toBeInTheDocument();
    expect(screen.getByText("Total Plastic Credits: 100")).toBeInTheDocument();
  });

  it("4. Shows wallet warning when trying to buy without wallet", async () => {
    vi.spyOn(mockStore, "getState").mockReturnValue({
      roadmaps: {
        roadmaps: [
          {
            preId: "pre-1",
            roadmapId: "roadmap-1",
            roadmapName: "Test Roadmap",
            roadmapDescription: "Test Description",
            progress: 0,
            adminPkh: "admin-pkh",
            prePkh: "pre-pkh",
            preSkh: "pre-skh",
            totalPlasticCredits: 100,
            soldPlasticCredits: 50,
            totalPlasticTokens: 0,
            sentPlasticTokens: 0,
            totalPlastic: 100,
            recoveredPlastic: 0,
          },
        ],
        loading: false,
        error: null,
      },
    });

    renderComponent(null);
    fireEvent.click(screen.getByText("Buy Now"));
    expect(toast.warning).toHaveBeenCalledWith(
      "Please connect your wallet before buying."
    );
  });
});
