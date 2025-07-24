// src/components/__tests__/RoadmapDetails.test.tsx
import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { render, screen, fireEvent, waitFor } from "@testing-library/react";
import { Provider } from "react-redux";
import { MemoryRouter } from "react-router-dom";
import { configureStore } from "@reduxjs/toolkit";
import RoadmapDetails from "../../../src/components/admin/RoadmapDetails";
import { WalletContext } from "../../../src/App";
import roadmapSlice from "../../../src/redux/roadmapSlice";
import completedRoadmapSlice from "../../../src/redux/completedRoadmapSlice";
import archivedRoadmapSlice from "../../../src/redux/archivedRoadmapSlice";
import transactionSlice, {
  TransactionType,
} from "../../../src/redux/TransactionSlice";
import adminSlice from "../../../src/redux/adminSlice";
import walletSlice from "../../../src/redux/walletSlice";
import authSlice from "../../../src/redux/authSlice";
import { BrowserWallet } from "@meshsdk/core";
import React from "react";
import "@testing-library/jest-dom";

// Mock external dependencies
vi.mock("axios");
vi.mock("@/services/cardano");
vi.mock("sonner", () => ({
  toast: {
    success: vi.fn(),
    error: vi.fn(),
  },
}));

// Mock the helper function for address truncation
vi.mock("@/utils/helper", () => ({
  truncateAddress: vi.fn(
    (address) => `${address.slice(0, 6)}...${address.slice(-4)}`
  ),
  formatAmount: vi.fn((amount) => amount.toString()),
}));

// Mock react-router-dom hooks
const mockNavigate = vi.fn();
vi.mock("react-router-dom", async () => {
  const actual = await vi.importActual("react-router-dom");
  return {
    ...actual,
    useNavigate: () => mockNavigate,
    useParams: () => ({ roadmapId: "test-roadmap-id" }),
  };
});

// Mock wallet context
const mockWallet = {
  _walletInstance: {},
  _walletName: "MockWallet",
  walletInstance: {},
  getChangeAddress: vi.fn(),
  getRewardAddresses: vi.fn(),
  getUnusedAddresses: vi.fn(),
  getUsedAddresses: vi.fn(),
  getUtxos: vi.fn(),
  signData: vi.fn(),
  signTx: vi.fn(),
  submitTx: vi.fn(),
  getBalance: vi.fn().mockResolvedValue([
    {
      unit: "token123",
      quantity: "5000",
    },
  ]),
  enable: vi.fn(),
  isEnabled: vi.fn(),
  getNetworkId: vi.fn(),
  getCollateral: vi.fn(),
  getExtensions: vi.fn(),
  getName: vi.fn(),
  getIcon: vi.fn(),
  getApiVersion: vi.fn(),
  getSupportedExtensions: vi.fn(),
  signTxs: vi.fn(),
  getUsedAddress: vi.fn(),
  getCollateralUnspentOutput: vi.fn(),
  getUsedUTxOs: vi.fn(),
} as unknown as BrowserWallet;

vi.mock("@meshsdk/core", () => ({
  BrowserWallet: vi.fn(() => mockWallet),
}));

// Test data
const mockRoadmap = {
  roadmapId: "test-roadmap-id",
  roadmapName: "Test Roadmap",
  roadmapDescription: "Test Roadmap Description",
  preAddress: "addr_pre_test123",
  progress: 75,
  recoveredPlastic: 100,
  sentPlasticTokens: 500000,
  totalPlasticTokens: 1000000,
  totalPlasticCredits: 1000,
  soldPlasticCredits: 750,
  fundsMissing: "250000000", // 250 USDM in micro
  fundsDistributed: "750000000", // 750 USDM in micro
  status: "active",
  preId: "pre-test-id",
  totalPlastic: 1000,
  createdAt: "2022-01-01",
};

const createMockStore = (initialState = {}) => {
  return configureStore({
    reducer: {
      roadmaps: roadmapSlice,
      completedRoadmaps: completedRoadmapSlice,
      archivedRoadmaps: archivedRoadmapSlice,
      transactions: transactionSlice,
      admin: adminSlice,
      wallet: walletSlice,
      auth: authSlice,
    },
    preloadedState: {
      roadmaps: {
        roadmaps: [mockRoadmap],
        loading: false,
        error: null,
      },
      completedRoadmaps: {
        roadmaps: [],
        loading: false,
        error: null,
      },
      archivedRoadmaps: {
        roadmaps: [],
        loading: false,
        error: null,
      },
      transactions: {
        transactions: [],
        page: 1,
        perPage: 10,
        total: 0,
        totalPages: 1,
        loading: false,
        error: null,
        currentFilter: [TransactionType.Sold],
      },
      admin: {
        admins: [],
        loading: false,
        error: null,
      },
      wallet: {
        walletId: null,
        walletAddress: "addr_test123",
      },
      auth: {
        role: null,
        isAuthenticated: true,
      },
      ...initialState,
    },
  });
};

const renderComponent = (
  store = createMockStore(),
  walletContext = mockWallet
) => {
  return render(
    <Provider store={store}>
      <WalletContext.Provider value={walletContext}>
        <MemoryRouter initialEntries={["/roadmap/test-roadmap-id"]}>
          <RoadmapDetails />
        </MemoryRouter>
      </WalletContext.Provider>
    </Provider>
  );
};

describe("RoadmapDetails", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("renders roadmap details correctly", () => {
    renderComponent();

    expect(screen.getByText("Test Roadmap")).toBeInTheDocument();
    expect(screen.getByText("Roadmap Details")).toBeInTheDocument();
    expect(screen.getByText("75%")).toBeInTheDocument();
    expect(screen.getByText("100 kg")).toBeInTheDocument();
    expect(screen.getByText("1000 USDM")).toBeInTheDocument();
  });

  it('shows "not found" message when roadmap does not exist', () => {
    const store = createMockStore({
      roadmaps: {
        roadmaps: [],
        loading: false,
        error: null,
      },
    });

    renderComponent(store);
    expect(screen.getByText("Roadmap not found")).toBeInTheDocument();
  });

  it("does not render admin controls for non-admin users", () => {
    const store = createMockStore({
      auth: {
        role: "USER",
        isAuthenticated: true,
      },
    });

    renderComponent(store);

    expect(screen.queryByText("Add Admin")).not.toBeInTheDocument();
    expect(screen.queryByText("Archive Roadmap")).not.toBeInTheDocument();
    expect(screen.queryByText("Release Funds")).not.toBeInTheDocument();
  });

  it("renders admin controls for admin users", () => {
    const store = createMockStore({
      auth: {
        role: "ADMIN",
        isAuthenticated: true,
      },
    });

    renderComponent(store);

    expect(screen.getByText("Add Admin")).toBeInTheDocument();
    expect(screen.getByText("Archive Roadmap")).toBeInTheDocument();
    expect(screen.getByText("Release Funds")).toBeInTheDocument();
  });

  it("renders correct transaction history", () => {
    renderComponent();

    expect(screen.getByText("Transaction History")).toBeInTheDocument();
    expect(
      screen.getByText(
        "Below is a record of recent transactions, providing details about key activities and their timestamps for better tracking and transparency."
      )
    ).toBeInTheDocument();
  });

  it("copies address to clipboard when clicked", async () => {
    // Mock clipboard API
    const writeTextMock = vi.fn().mockResolvedValue(void 0);
    Object.assign(navigator, {
      clipboard: {
        writeText: writeTextMock,
      },
    });

    renderComponent();

    // Look for the truncated address instead of the full address
    const addressElement = screen.getByText("addr_p...t123");
    fireEvent.click(addressElement);

    expect(writeTextMock).toHaveBeenCalledWith("addr_pre_test123");
  });
});
