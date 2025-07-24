import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen, fireEvent, waitFor } from "@testing-library/react";
import '@testing-library/jest-dom'; // Import jest-dom matchers
import Transactions from "../../../src/pages/dashboard/transactions";
import { Provider } from "react-redux";
import { store } from "../../../src/redux/store";
import React from "react";
import { TransactionType } from "../../../src/redux/TransactionSlice";

// ✅ Mocks
vi.mock("../../../src/components/StatsCard", () => ({
  default: ({ title, value }: any) => (
    <div data-testid="stats-card">
      <h3>{title}</h3>
      <p>{value}</p>
    </div>
  ),
}));

vi.mock("../../../src/components/DynamicTable", () => ({
  default: ({ transactions }: any) => (
    <div data-testid="dynamic-table">
      {transactions.length === 0 ? "No Transactions" : "Transactions Table"}
    </div>
  ),
}));

vi.mock("../../../src/components/CheckboxFilterList", () => ({
  __esModule: true,
  default: ({ options, onChange }: any) => (
    <div data-testid="checkbox-list">
      {options.map((opt: any) => (
        <button key={opt.id} onClick={() => onChange([opt.id])}>
          {opt.label}
        </button>
      ))}
    </div>
  ),
}));

// ✅ Default Props
const defaultProps = {
  transactions: [
    {
      id: "1",
      type: TransactionType.Sold,
      description: "Test transaction",
      date: "2023-07-21",
    },
  ],
  loading: false,
  error: null,
  page: 1,
  totalPages: 3,
  onPageChange: vi.fn(),
};

const renderComponent = (customProps = {}) => {
  const props = { ...defaultProps, ...customProps };
  return render(
    <Provider store={store}>
      <Transactions {...props} />
    </Provider>
  );
};

describe("Transactions Page", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("renders stats cards", () => {
    renderComponent();

    const statCards = screen.getAllByTestId("stats-card");
    expect(statCards.length).toBeGreaterThan(0);
  });

  it("shows transaction table when not loading", () => {
    renderComponent();
    expect(screen.getByTestId("dynamic-table")).toBeInTheDocument();
    expect(screen.getByText("Transactions Table")).toBeInTheDocument();
  });

  it("shows loading spinner", () => {
    renderComponent({ loading: true });

    expect(screen.getByText("Loading transactions...")).toBeInTheDocument();
  });

  it("shows error message", () => {
    renderComponent({ error: "Something went wrong" });

    expect(screen.getByText("Something went wrong")).toBeInTheDocument();
  });

  it("shows default error message for unknown error", () => {
    renderComponent({ error: {} });

    expect(
      screen.getByText("Failed to load transactions.")
    ).toBeInTheDocument();
  });

  it("toggles filter dropdown", async () => {
    renderComponent();

    const filterButton = screen.getByText("Filter By");
    fireEvent.click(filterButton);

    expect(await screen.findByTestId("checkbox-list")).toBeInTheDocument();
  });

  it("changes filter and calls onPageChange", async () => {
    const onPageChange = vi.fn();
    renderComponent({ onPageChange });

    const filterButton = screen.getByText("Filter By");
    fireEvent.click(filterButton);

    const pcSaleFilter = await screen.findByText("Plastic Credit Sale");
    fireEvent.click(pcSaleFilter);

    await waitFor(() => {
      expect(onPageChange).toHaveBeenCalledWith(1, TransactionType.Sold);
    });
  });

  it("calls onPageChange when page is changed", async () => {
    const onPageChange = vi.fn();
    renderComponent({ onPageChange });

    const nextButton = screen.getByText("Next");
    fireEvent.click(nextButton);

    await waitFor(() => {
      expect(onPageChange).toHaveBeenCalledWith(2, TransactionType.Sold);
    });
  });
});
