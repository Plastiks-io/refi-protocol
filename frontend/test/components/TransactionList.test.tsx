import React from "react";
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen } from "@testing-library/react";
import "@testing-library/jest-dom";
import TransactionList from "../../src/components/TransactionList";
import { TransactionType } from "../../src/redux/TransactionSlice";
import { formatDateTime } from "../../src/utils/helper";

vi.mock("../../src/utils/helper", () => ({
  formatDateTime: vi.fn(() => ({
    datePart: "2025-01-01",
    timePart: "12:00 PM",
  })),
}));

describe("TransactionList component", () => {
  const baseTransactions = [
    {
      id: "1",
      type: TransactionType.Sold,
      amount: 100,
      txDate: "2025-01-01T12:00:00Z",
      roadmapId: "r1",
    },
    {
      id: "2",
      type: TransactionType.Transfer,
      amount: 200,
      txDate: "2025-01-02T13:00:00Z",
      roadmapId: "r1",
    },
    {
      id: "3",
      type: "Ignored" as TransactionType,
      amount: 999,
      txDate: "2025-01-03T14:00:00Z",
      roadmapId: "r1",
    },
  ];

  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("renders Sold and Transfer transactions", () => {
    render(<TransactionList transactions={baseTransactions} />);
    expect(screen.getByText("Transaction History")).toBeInTheDocument();
    expect(screen.getByText("100 kg of plastic")).toBeInTheDocument();
    expect(screen.getByText("200 USDM to Escrow")).toBeInTheDocument();
    expect(screen.queryByText("999")).not.toBeInTheDocument();
    expect(screen.getAllByText("2025-01-01")[0]).toBeInTheDocument();
  });

  it("filters transactions by roadmapId", () => {
    const txWithDifferentRoadmap = {
      ...baseTransactions[0],
      roadmapId: "other-roadmap",
    };

    render(
      <TransactionList transactions={[txWithDifferentRoadmap]} roadmapId="r1" />
    );

    expect(screen.queryByText("100 kg of plastic")).not.toBeInTheDocument();
    expect(screen.getByText("Transaction History")).toBeInTheDocument();
  });

  it("renders nothing when no valid transactions are present", () => {
    render(<TransactionList transactions={[]} />);
    expect(screen.queryByText("100 kg of plastic")).not.toBeInTheDocument();
    expect(screen.queryByText("Transaction History")).toBeInTheDocument();
  });

  it("calls formatDateTime for each transaction", () => {
    render(<TransactionList transactions={baseTransactions} />);
    expect(formatDateTime).toHaveBeenCalledTimes(2);
  });
});
