import React from "react";
import { render, screen } from "@testing-library/react";
import DynamicTable from "../../src/components/Table";
import { describe, it, expect } from "vitest";
import { Transaction } from "../../src/redux/TransactionSlice";

const mockTransactions: Transaction[] = [
  {
    date: "05/11/2025",
    transactionFee: "0.17 ADA",
    amount: "10 ADA",
    tokenId: "123456789",
    direction: "received",
    pcAssetId: "https://abcd",
    hash: "https://abcd",
  },
];

describe("DynamicTable", () => {
  it("renders 'No transactions available.' when transactions is empty", () => {
    render(<DynamicTable transactions={[]} />);
    expect(screen.getByText(/No transactions available/i)).toBeInTheDocument();
  });

  it("renders table headers based on transaction keys", () => {
    render(<DynamicTable transactions={mockTransactions} />);
    expect(screen.getByText(/date/i)).toBeInTheDocument();
    expect(screen.getByText(/transaction fee/i)).toBeInTheDocument();
    expect(screen.getByText(/amount/i)).toBeInTheDocument();
    expect(screen.getByText(/token id/i)).toBeInTheDocument();
    expect(screen.getByText(/direction/i)).toBeInTheDocument();
    expect(screen.getByText(/pc asset id/i)).toBeInTheDocument();
    expect(screen.getAllByText(/hash/i).length).toBeGreaterThan(0);
  });

  it("renders all transaction rows", () => {
    render(<DynamicTable transactions={mockTransactions} />);
    expect(screen.getAllByRole("row")).toHaveLength(
      mockTransactions.length + 1
    ); // +1 for header row
  });

  it("renders string values as plain text", () => {
    render(<DynamicTable transactions={mockTransactions} />);
    expect(screen.getByText("05/11/2025")).toBeInTheDocument();
    expect(screen.getByText("0.17 ADA")).toBeInTheDocument();
    expect(screen.getByText("10 ADA")).toBeInTheDocument();
    expect(screen.getByText("received")).toBeInTheDocument();
  });

  it("renders hash as a link if it starts with http", async () => {
    render(<DynamicTable transactions={mockTransactions} />);
    const link = await screen.findAllByText("Link");
    expect(link[0]).toBeInTheDocument();
    expect(link[0].tagName).toBe("A");
    expect(link[0]).toHaveAttribute("href", "https://abcd");
  });

  it("renders nothing but the message if transactions is undefined", () => {
    // @ts-expect-error testing undefined
    render(<DynamicTable transactions={undefined} />);
    expect(screen.getByText(/No transactions available/i)).toBeInTheDocument();
    expect(screen.queryByRole("table")).not.toBeInTheDocument();
  });
});
