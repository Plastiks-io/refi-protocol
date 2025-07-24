// tests/components/admin/SettingsComponent.test.tsx
import { render, screen, waitFor, fireEvent } from "@testing-library/react";
import { vi } from "vitest";
import { Provider } from "react-redux";
import configureStore from "redux-mock-store";
import SettingsComponent from "../../../src/components/admin/Setting";
import { WalletContext } from "../../../src/App";
import { BrowserWallet } from "@meshsdk/core";
import React from "react";

// Mock modules
vi.mock("axios");
vi.mock("../../../src/services/cardano", () => ({
  cardanoClient: {
    depositPlastik: vi.fn().mockResolvedValue("tx123"),
    withdrawPlastik: vi.fn().mockResolvedValue("tx456"),
    redeemReward: vi.fn().mockResolvedValue("tx789"),
  },
}));

const mockStore = configureStore([]);

describe("SettingsComponent", () => {
  // Initial mock data
  const mockArchivedRoadmaps = [
    {
      id: "1",
      roadmapName: "Archived Roadmap 1",
      preId: "Entity 1",
      dateArchived: "2023-01-01T00:00:00Z",
    },
  ];

  const mockAdmins = [
    {
      id: "admin1",
      address: "0x1234567890abcdef1234567890abcdef12345678",
      role: "ADMIN",
      createdAt: "2023-01-01T00:00:00Z",
    },
    {
      id: "superadmin",
      address: "0xcurrentUser",
      role: "SUPER_ADMIN",
      createdAt: "2023-01-01T00:00:00Z",
    },
  ];

  const initialState = {
    archivedRoadmaps: {
      roadmaps: mockArchivedRoadmaps,
      loading: false,
      error: null,
    },
    admin: {
      admins: mockAdmins,
    },
    wallet: {
      walletAddress: "0xcurrentUser",
    },
  };

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

  const renderComponent = (stateOverrides = {}) => {
    const store = mockStore({ ...initialState, ...stateOverrides });
    return render(
      <Provider store={store}>
        <WalletContext.Provider value={mockWallet}>
          <SettingsComponent />
        </WalletContext.Provider>
      </Provider>
    );
  };

  beforeEach(() => {
    vi.clearAllMocks();
  });

  // Test Cases
  it("renders archived roadmaps correctly", () => {
    renderComponent();
    expect(screen.getAllByText("Archived Roadmaps").length).toBe(2);
    expect(screen.getByText("Archived Roadmap 1")).toBeInTheDocument();
    expect(screen.getByText("Entity 1")).toBeInTheDocument();
    expect(screen.getByText("Jan 1, 2023")).toBeInTheDocument();
  });

  it("switches to admin tab and shows Add Admin button", () => {
    renderComponent();
    fireEvent.click(screen.getByText("Admin Permissions"));
    expect(screen.getByText("Add Admin")).toBeInTheDocument();
  });

  it("displays archived roadmaps correctly", () => {
    renderComponent();
    expect(screen.getByText("Archived Roadmap 1")).toBeInTheDocument();
    expect(screen.getByText("Entity 1")).toBeInTheDocument();
    expect(screen.getByText("Jan 1, 2023")).toBeInTheDocument();
  });

  it("disables Remove button for current user", () => {
    renderComponent();
    fireEvent.click(screen.getByText("Admin Permissions"));
    const removeButtons = screen.getAllByText(/Remove|You cannot remove/);
    expect(removeButtons[1]).toHaveTextContent(
      "You cannot remove yourself or super admin"
    );
  });
});
