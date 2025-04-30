import React from "react";
import { describe, it, expect, vi } from "vitest";
import { render, screen, fireEvent } from "@testing-library/react";
import Transactions from "../../../src/pages/dashboard/transactions";

vi.mock("../../../src/components/Table", () => ({
  __esModule: true,
  default: ({ transactions }: any) => (
    <div data-testid="dynamic-table">{JSON.stringify(transactions)}</div>
  ),
}));

vi.mock("../../../src/components/StatsCard", () => ({
  __esModule: true,
  default: ({ title, value, description }: any) => (
    <div data-testid="stats-card">
      <span>{title}</span>
      <span>{value}</span>
      <span>{description}</span>
    </div>
  ),
}));

describe("Transactions Page", () => {
  const baseProps = {
    transactions: [
      {
        date: "05/11/2025",
        transactionFee: "0.17 ADA",
        amount: "10 ADA",
        tokenId: "123456789",
        direction: "received",
        pcAssetId: "https://abcd",
        hash: "https://abcd",
      },
    ],
    loading: false,
    error: null,
    page: 2,
    count: 10,
    onPageChange: vi.fn(),
  };

  it("renders stats cards", () => {
    render(<Transactions {...baseProps} />);
    const statsCards = screen.getAllByTestId("stats-card");
    expect(statsCards.length).toBe(2);
    expect(statsCards[0]).toHaveTextContent("Total Tokens Retired");
    expect(statsCards[1]).toHaveTextContent("Plastic Recovered");
  });

  it("renders table with transactions", () => {
    render(<Transactions {...baseProps} />);
    expect(screen.getByTestId("dynamic-table")).toHaveTextContent(
      JSON.stringify(baseProps.transactions)
    );
  });

  it("renders loading state", () => {
    render(<Transactions {...baseProps} loading={true} />);
    expect(screen.getByText("Loading transactions...")).toBeInTheDocument();
  });

  it("renders error state with string error", () => {
    render(<Transactions {...baseProps} error="Something went wrong" />);
    expect(screen.getByText("Something went wrong")).toBeInTheDocument();
  });

  it("renders error state with non-string error", () => {
    render(<Transactions {...baseProps} error={{}} />);
    expect(
      screen.getByText("Failed to load transactions.")
    ).toBeInTheDocument();
  });

  it("calls onPageChange with previous page", () => {
    render(<Transactions {...baseProps} />);
    const prevBtn = screen.getByText("Previous");
    fireEvent.click(prevBtn);
    expect(baseProps.onPageChange).toHaveBeenCalledWith(1);
  });

  it("calls onPageChange with next page", () => {
    render(<Transactions {...baseProps} />);
    const nextBtn = screen.getByText("Next");
    fireEvent.click(nextBtn);
    expect(baseProps.onPageChange).toHaveBeenCalledWith(1);
  });

  it("disables Previous button on first page", () => {
    render(<Transactions {...baseProps} page={1} />);
    expect(screen.getByText("Previous")).toBeDisabled();
  });

  it("disables Next button if transactions.length < count", () => {
    render(
      <Transactions {...baseProps} transactions={[{ id: 1 }]} count={10} />
    );
    expect(screen.getByText("Next")).toBeDisabled();
  });

  it("shows correct headings and description", () => {
    render(<Transactions {...baseProps} />);
    expect(screen.getByText("Transactions")).toBeInTheDocument();
    expect(
      screen.getByText(/Below is a detailed list of transactions history/i)
    ).toBeInTheDocument();
  });
});
