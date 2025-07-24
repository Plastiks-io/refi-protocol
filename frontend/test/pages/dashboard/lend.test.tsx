import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen, fireEvent, waitFor } from "@testing-library/react";
import Lend from "../../../src/pages/dashboard/lend";
import { WalletContext } from "../../../src/App";
import { useCardanoData } from "../../../src/contexts/cardanoContexts";
import { Provider } from "react-redux";
import { store } from "../../../src/redux/store";
import "@testing-library/jest-dom";
import React from "react";
import { BrowserWallet } from "@meshsdk/core";

// ✅ Mock axios (including .create)
vi.mock("axios", () => {
  const postMock = vi.fn();
  const createMock = vi.fn(() => ({
    post: postMock,
  }));

  return {
    default: {
      post: postMock,
      create: createMock,
    },
  };
});

// ✅ Mock sonner (toast)
vi.mock("sonner", () => ({
  toast: {
    success: vi.fn(),
    error: vi.fn(),
  },
}));

// ✅ Mock lucide-react (icons)
vi.mock("lucide-react", async () => {
  const actual = await vi.importActual("lucide-react");
  return {
    ...actual,
    Loader2: () => <div data-testid="loader">Loading...</div>,
  };
});

// ✅ Mock cardano client
vi.mock("../../../src/services/cardano", () => ({
  cardanoClient: {
    depositPlastik: vi.fn().mockResolvedValue("tx123"),
    withdrawPlastik: vi.fn().mockResolvedValue("tx456"),
    redeemReward: vi.fn().mockResolvedValue("tx789"),
  },
}));

// ✅ Mock Cardano context
vi.mock("../../../src/contexts/cardanoContexts", () => {
  return {
    useCardanoData: vi.fn(),
  };
});

// ✅ Mock socket
vi.mock("../../../src/socket", () => ({
  socket: {
    on: vi.fn(),
    off: vi.fn(),
  },
}));

// ✅ Mock wallet
const mockWallet = {
  // Required properties and methods for BrowserWallet type
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
  // Add any other required properties/methods as needed by BrowserWallet
} as unknown as BrowserWallet;

beforeEach(() => {
  vi.resetAllMocks();

  (import.meta as any).env.VITE_PLASTIC_TOKEN = "token123";
  (import.meta as any).env.VITE_SERVER_URL = "http://localhost:8080";
  (useCardanoData as any).mockReturnValue({
    data: {
      staked: BigInt(5000),
      rewardDebt: BigInt(1500000), // 1.5 USDM
    },
    refresh: vi.fn(),
  });
});

const renderComponent = (wallet = mockWallet) => {
  return render(
    <Provider store={store}>
      <WalletContext.Provider value={wallet}>
        <Lend />
      </WalletContext.Provider>
    </Provider>
  );
};

// ✅ TEST SUITE
describe("Lend Page", () => {
  it("shows loading spinner when data is null", () => {
    (useCardanoData as any).mockReturnValue({
      data: null,
      refresh: vi.fn(),
    });

    renderComponent();
    expect(screen.getByTestId("loader")).toBeInTheDocument();
  });

  it("renders lending and reward stats", async () => {
    renderComponent();

    expect(
      await screen.findByText("Put your PLASTIK tokens to use")
    ).toBeInTheDocument();
    expect(screen.getByText("5,000")).toBeInTheDocument();
    expect(screen.getByText("1.500000")).toBeInTheDocument();
  });

  it("opens and closes lend modal", async () => {
    renderComponent();

    fireEvent.click(screen.getByText("Lend Tokens"));
    expect(await screen.findByText("Lend PLASTIK Tokens")).toBeInTheDocument();

    fireEvent.click(screen.getByText("Cancel"));
    await waitFor(() => {
      expect(screen.queryByText("Lend PLASTIK Tokens")).not.toBeInTheDocument();
    });
  });

  it("confirms lend action", async () => {
    renderComponent();

    fireEvent.click(screen.getByText("Lend Tokens"));

    const input = await screen.findByPlaceholderText("Enter amount to lend");
    fireEvent.change(input, { target: { value: "200" } });

    fireEvent.click(screen.getByText("Confirm"));

    await waitFor(() => {
      expect(screen.queryByText("Lend PLASTIK Tokens")).not.toBeInTheDocument();
    });
  });

  it("opens and cancels withdrawal modal", async () => {
    renderComponent();

    fireEvent.click(screen.getByText("Withdraw"));
    expect(
      await screen.findByText("Withdraw PLASTIK Tokens")
    ).toBeInTheDocument();

    fireEvent.click(screen.getByText("Cancel"));
    await waitFor(() =>
      expect(
        screen.queryByText("Withdraw PLASTIK Tokens")
      ).not.toBeInTheDocument()
    );
  });

  it("does not redeem reward if none available", async () => {
    const { toast } = await import("sonner");

    (useCardanoData as any).mockReturnValue({
      data: {
        staked: BigInt(1000),
        rewardDebt: BigInt(0),
      },
      refresh: vi.fn(),
    });

    renderComponent();
    fireEvent.click(screen.getByText("Redeem Rewards"));

    await waitFor(() => {
      expect(toast.error).toHaveBeenCalledWith(
        "No rewards available to redeem",
        expect.any(Object)
      );
    });
  });

  it("disables lending for invalid input", async () => {
    renderComponent();

    fireEvent.click(screen.getByText("Lend Tokens"));

    const input = await screen.findByPlaceholderText("Enter amount to lend");
    fireEvent.change(input, { target: { value: "-10" } });

    fireEvent.click(screen.getByText("Confirm"));

    await waitFor(() => {
      expect(screen.getByText("Lend PLASTIK Tokens")).toBeInTheDocument();
    });
  });

  it("disables withdraw for invalid amount", async () => {
    renderComponent();

    fireEvent.click(screen.getByText("Withdraw"));

    const input = await screen.findByPlaceholderText(
      "Enter amount to withdraw"
    );
    fireEvent.change(input, { target: { value: "-100" } });

    fireEvent.click(screen.getByText("Confirm"));

    await waitFor(() => {
      expect(screen.getByText("Withdraw PLASTIK Tokens")).toBeInTheDocument();
    });
  });
});
