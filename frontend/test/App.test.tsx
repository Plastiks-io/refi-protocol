import React from "react";
import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { render, screen } from "@testing-library/react";
import "@testing-library/jest-dom";

vi.mock("react-redux", () => {
  const actual = require("react-redux");
  return {
    ...actual,
    useDispatch: () => vi.fn(),
    useSelector: (selector) =>
      selector({
        wallet: { walletId: "mock-wallet-id" },
        transactions: {
          transactions: [],
          loading: false,
          error: null,
          page: 1,
          perPage: 10,
          totalPages: 1,
        },
        auth: { role: "USER" },
      }),
  };
});

vi.mock("../src/pages/dashboard", () => ({
  default: () => <div data-testid="dashboard">Dashboard</div>,
}));

vi.mock("../src/components/Navbar", () => ({
  default: () => <div data-testid="navbar">Navbar</div>,
}));

vi.mock("../src/components/Footer", () => ({
  default: () => <div data-testid="footer">Footer</div>,
}));

vi.mock("../src/pages/dashboard/transactions", () => ({
  default: () => <div data-testid="transactions">Transactions</div>,
}));

vi.mock("../src/pages/dashboard/community", () => ({
  default: () => <div data-testid="community">Community</div>,
}));

vi.mock("../src/pages/dashboard/admin", () => ({
  default: () => <div data-testid="admin">Admin</div>,
}));

vi.mock("../src/pages/user/nft", () => ({
  default: () => <div data-testid="nft-purchase">NFT Purchase</div>,
}));

vi.mock("../src/pages/dashboard/lend", () => ({
  default: () => <div data-testid="lend">Lend</div>,
}));

vi.mock("../src/components/admin/RoadmapDetails", () => ({
  default: () => <div data-testid="roadmap-details">Roadmap Details</div>,
}));

vi.mock("../src/components/admin/Setting", () => ({
  default: () => <div data-testid="settings">Settings</div>,
}));

vi.mock("../src/components/PrivateAdminRoute", () => ({
  default: ({ children }) => (
    <div data-testid="private-admin-route">{children}</div>
  ),
}));

vi.mock("../src/components/PrivateLendRoute", () => ({
  default: ({ children }) => (
    <div data-testid="private-lend-route">{children}</div>
  ),
}));

vi.mock("../src/socket/SocketManager", () => ({
  default: () => <div data-testid="socket-manager">Socket Manager</div>,
}));

vi.mock("@/contexts/cardanoContexts", () => ({
  CardanoProvider: ({ children }) => (
    <div data-testid="cardano-provider">{children}</div>
  ),
}));

vi.mock("../src/redux/roadmapSlice", () => ({
  fetchRoadmaps: vi.fn(() => ({ type: "roadmap/fetchRoadmaps" })),
}));

vi.mock("../src/redux/completedRoadmapSlice", () => ({
  fetchCompletedRoadmaps: vi.fn(() => ({
    type: "completedRoadmap/fetchCompletedRoadmaps",
  })),
}));

vi.mock("../src/redux/archivedRoadmapSlice", () => ({
  fetchArchivedRoadmaps: vi.fn(() => ({
    type: "archivedRoadmap/fetchArchivedRoadmaps",
  })),
}));

vi.mock("../src/redux/TransactionSlice", () => ({
  fetchTransactions: vi.fn(() => ({ type: "transactions/fetchTransactions" })),
  TransactionType: {},
}));

vi.mock("../src/redux/adminSlice", () => ({
  fetchAdmins: vi.fn(() => ({ type: "admin/fetchAdmins" })),
}));

import App from "../src/App";

describe("App Component", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("renders main layout components", () => {
    render(<App />);
    expect(screen.getByTestId("navbar")).toBeInTheDocument();
    expect(screen.getByTestId("footer")).toBeInTheDocument();
    expect(screen.getByTestId("socket-manager")).toBeInTheDocument();
    expect(screen.getByTestId("cardano-provider")).toBeInTheDocument();
  });

  it("renders Dashboard on root path", () => {
    window.history.pushState({}, "Dashboard", "/");
    render(<App />);
    expect(screen.getByTestId("dashboard")).toBeInTheDocument();
  });

  it("renders Transactions page", () => {
    window.history.pushState({}, "Transactions", "/transactions");
    render(<App />);
    expect(screen.getByTestId("transactions")).toBeInTheDocument();
  });

  it("renders Community page", () => {
    window.history.pushState({}, "Community", "/community");
    render(<App />);
    expect(screen.getByTestId("community")).toBeInTheDocument();
  });

  it("renders Admin page with protection", () => {
    window.history.pushState({}, "Admin", "/admin");
    render(<App />);
    expect(screen.getByTestId("private-admin-route")).toBeInTheDocument();
    expect(screen.getByTestId("admin")).toBeInTheDocument();
  });

  it("renders Lend page with protection", () => {
    window.history.pushState({}, "Lend", "/lend");
    render(<App />);
    expect(screen.getByTestId("private-lend-route")).toBeInTheDocument();
    expect(screen.getByTestId("lend")).toBeInTheDocument();
  });

  it("renders NFT Purchase page", () => {
    window.history.pushState({}, "Buy NFT", "/buy-nft");
    render(<App />);
    expect(screen.getByTestId("nft-purchase")).toBeInTheDocument();
  });

  it("renders Settings page for admin", () => {
    window.history.pushState({}, "Admin Settings", "/admin/settings");
    render(<App />);
    expect(screen.getByTestId("private-admin-route")).toBeInTheDocument();
    expect(screen.getByTestId("settings")).toBeInTheDocument();
  });
});
