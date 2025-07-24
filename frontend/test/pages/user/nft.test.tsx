import React from "react";
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen, fireEvent, waitFor } from "@testing-library/react";
import NFTPurchase from "../../../src/pages/user/nft";
import { WalletContext } from "../../../src/App";
import { Provider } from "react-redux";
import { store } from "../../../src/redux/store";
import { useSelector } from "react-redux";
import { BrowserWallet } from "@meshsdk/core";
import "@testing-library/jest-dom";

// 🧪 Mock Redux & Dispatch
vi.mock("react-redux", async () => {
  const actual = await vi.importActual("react-redux");
  return {
    ...actual,
    useSelector: vi.fn(),
    useDispatch: () => vi.fn(),
  };
});

// 🧪 Mock toast
vi.mock("sonner", () => ({
  toast: {
    error: vi.fn(),
    warning: vi.fn(),
  },
}));

// 🧪 Mock Cardano client
vi.mock("../../../src/services/cardano", () => ({
  cardanoClient: {
    sentPc: vi.fn().mockResolvedValue("mocked-tx"),
  },
}));

// 🌱 Mock wallet context
const mockWallet = {
  getChangeAddress: vi
    .fn()
    .mockResolvedValue(
      "addr_test1qxyz43reffqfeqwfewqweqfer32rqwascdqwedaswqeads"
    ),
} as unknown as BrowserWallet;

const mockRoadmaps = [
  {
    roadmapId: "1",
    roadmapName: "Recycle Ocean Waste",
    roadmapDescription: "Removing plastics from oceans",
    totalPlasticCredits: 10,
    soldPlasticCredits: 2,
    totalPlasticTokens: 100,
    sentPlasticTokens: 50,
    totalPlastic: 500,
    recoveredPlastic: 100,
    preId: "pre123",
  },
];

beforeEach(() => {
  vi.clearAllMocks();

  (useSelector as any).mockImplementation((selectorFn: any) =>
    selectorFn({
      roadmaps: {
        roadmaps: mockRoadmaps,
        loading: false,
        error: null,
      },
    })
  );
});

const renderComponent = (wallet = mockWallet) => {
  return render(
    <Provider store={store}>
      <WalletContext.Provider value={wallet}>
        <NFTPurchase />
      </WalletContext.Provider>
    </Provider>
  );
};

describe("NFTPurchase Component", () => {
  it("renders roadmap cards", async () => {
    renderComponent();
    expect(await screen.findByText("Recycle Ocean Waste")).toBeInTheDocument();
    expect(screen.getByPlaceholderText("Enter amount")).toBeInTheDocument();
    expect(screen.getByText("Buy Now")).toBeInTheDocument();
  });

  it("shows loading screen when loading is true", () => {
    (useSelector as any).mockImplementation((selectorFn: any) =>
      selectorFn({
        roadmaps: {
          roadmaps: [],
          loading: true,
          error: null,
        },
      })
    );
    renderComponent();
    expect(screen.getByRole("status")).toBeInTheDocument();
  });

  it("shows error screen when error exists", () => {
    (useSelector as any).mockImplementation((selectorFn: any) =>
      selectorFn({
        roadmaps: {
          roadmaps: [],
          loading: false,
          error: "Something broke",
        },
      })
    );
    renderComponent();
    expect(screen.getByText("Error: Something broke")).toBeInTheDocument();
  });

  it("shows warning if wallet not connected", async () => {
    const toast = await import("sonner");

    render(
      <Provider store={store}>
        <WalletContext.Provider value={null}>
          <NFTPurchase />
        </WalletContext.Provider>
      </Provider>
    );

    const button = screen.getByTestId("buy-now-button");
    fireEvent.click(button);

    await waitFor(() => {
      expect(toast.toast.warning).toHaveBeenCalledWith(
        "Please connect your wallet before buying.",
        expect.anything()
      );
    });
  });
});
