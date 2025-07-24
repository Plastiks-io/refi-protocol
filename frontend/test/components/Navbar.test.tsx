import { useSelector } from "react-redux";
import { signOutOnServer } from "../../src/services/auth";
import React from "react";
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen, fireEvent, waitFor } from "@testing-library/react";
import "@testing-library/jest-dom";
import Navbar from "../../src/components/Navbar";
import { BrowserRouter } from "react-router-dom";

vi.mock("@/services/auth", () => ({
  signInOnServer: vi.fn(() =>
    Promise.resolve({ email: "test@admin.com", role: "ADMIN" })
  ),
  signOutOnServer: vi.fn(() => Promise.resolve()),
}));

vi.mock("@meshsdk/core", () => ({
  BrowserWallet: {
    getAvailableWallets: vi.fn(() => Promise.resolve([{ id: "nami" }])),
    enable: vi.fn(() => ({
      getChangeAddress: vi.fn(() => Promise.resolve("addr_test123456789")),
    })),
  },
}));

vi.mock("react-redux", async () => {
  const actual = await vi.importActual("react-redux");
  return {
    ...actual,
    useSelector: vi.fn(),
    useDispatch: () => vi.fn(),
  };
});

vi.mock("@/assets/icons", () => ({
  PlastikLogo: "mockLogo.png",
}));

vi.mock("@/components/Popup", () => ({
  __esModule: true,
  default: ({ wallets, onSelectWallet, onClose }: any) => (
    <div data-testid="wallet-popup">
      <button onClick={() => onSelectWallet("nami")}>Mock Wallet</button>
      <button onClick={onClose}>Close</button>
    </div>
  ),
}));

vi.mock("@/components/admin/DisconnectPopup", () => ({
  __esModule: true,
  default: ({ isOpen, onConfirm, onCancel }: any) =>
    isOpen ? (
      <div data-testid="disconnect-popup">
        <button onClick={onConfirm}>Confirm Disconnect</button>
        <button onClick={onCancel}>Cancel</button>
      </div>
    ) : null,
}));

describe("Navbar Component", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  const renderWithRouter = () =>
    render(
      <BrowserRouter>
        <Navbar />
      </BrowserRouter>
    );

  it("shows Connect Wallet button if not connected", () => {
    (useSelector as any).mockImplementation((selector: any) =>
      selector({
        wallet: { walletAddress: null },
        auth: { role: null },
      })
    );

    renderWithRouter();

    expect(screen.getByText("Connect Wallet")).toBeInTheDocument();
  });

  it("shows admin links if user is ADMIN", () => {
    (useSelector as any).mockImplementation((selector: any) =>
      selector({
        wallet: { walletAddress: "addr_test1..." },
        auth: { role: "ADMIN" },
      })
    );

    renderWithRouter();

    expect(screen.getByText("Admin")).toBeInTheDocument();
  });

  it("opens popup and selects wallet", async () => {
    (useSelector as any).mockImplementation((selector: any) =>
      selector({
        wallet: { walletAddress: null },
        auth: { role: null },
      })
    );

    renderWithRouter();

    fireEvent.click(screen.getByText("Connect Wallet"));

    await waitFor(() =>
      expect(screen.getByTestId("wallet-popup")).toBeInTheDocument()
    );

    fireEvent.click(screen.getByText("Mock Wallet"));

    await waitFor(() =>
      expect(screen.queryByTestId("wallet-popup")).not.toBeInTheDocument()
    );
  });

  it("shows wallet address and disconnect button if connected", () => {
    (useSelector as any).mockImplementation((selector: any) =>
      selector({
        wallet: { walletAddress: "addr_test123456789" },
        auth: { role: "USER" },
      })
    );

    renderWithRouter();

    expect(screen.getByText("Disconnect")).toBeInTheDocument();
    expect(screen.getByText(/addr_/)).toBeInTheDocument();
  });

  it("calls signOutOnServer when disconnect clicked", async () => {
    (useSelector as any).mockImplementation((selector: any) =>
      selector({
        wallet: { walletAddress: "addr_test123456789" },
        auth: { role: "USER" },
      })
    );

    renderWithRouter();

    fireEvent.click(screen.getByText("Disconnect"));

    await waitFor(() => expect(signOutOnServer).toHaveBeenCalled());
  });

  it("opens mobile menu when menu button clicked", () => {
    (useSelector as any).mockImplementation((selector: any) =>
      selector({
        wallet: { walletAddress: null },
        auth: { role: null },
      })
    );

    renderWithRouter();

    fireEvent.click(screen.getByTestId("menu-button"));

    expect(screen.getByTestId("mobile-menu")).toBeInTheDocument();
  });
});
