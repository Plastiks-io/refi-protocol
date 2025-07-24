import React from "react";
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen } from "@testing-library/react";
import "@testing-library/jest-dom";
import { MemoryRouter } from "react-router-dom";

import DynamicTable from "../../src/components/DynamicTable";
import { Transaction, TransactionType } from "../../src/redux/TransactionSlice";

const mockTransactions: Transaction[] = [
  {
    id: "tx1",
    txDate: new Date(),
    txFee: 1.2,
    amount: 100,
    assetId: "asset1234567890",
    hash: "hashabc123456",
    roadmapId: "road1",
    type: TransactionType.Sold,
    createdAt: new Date(),
  },
];

describe("DynamicTable component", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("renders table headers correctly for Plastic Credit", () => {
    render(
      <MemoryRouter>
        <DynamicTable
          transactions={mockTransactions}
          filterValue="Plastic Credit"
        />
      </MemoryRouter>
    );

    expect(screen.getByText("Transaction Date")).toBeInTheDocument();
    expect(screen.getByText("Transaction Fee")).toBeInTheDocument();
    expect(screen.getByText("Sale Amount (USD)")).toBeInTheDocument();
    expect(screen.getByText("Token ID")).toBeInTheDocument();
    expect(screen.getByText("Plastic Credit")).toBeInTheDocument();
    expect(screen.getByText("Hash")).toBeInTheDocument();
  });

  it("renders transaction rows with formatted date and data", () => {
    render(
      <MemoryRouter>
        <DynamicTable transactions={mockTransactions} filterValue="Plastik" />
      </MemoryRouter>
    );

    expect(screen.getByText("1.2%")).toBeInTheDocument();
    expect(screen.getByText("100")).toBeInTheDocument();
    expect(screen.getByText("asset1234567890")).toBeInTheDocument();

    const plastikLinks = screen.getAllByText("Link to Plastik");
    expect(plastikLinks[0]).toHaveAttribute(
      "href",
      "https://preprod.cexplorer.io/asset/asset1234567890"
    );
  });

  it("applies correct hrefs to asset and transaction links", () => {
    render(
      <MemoryRouter>
        <DynamicTable transactions={mockTransactions} filterValue="USDM" />
      </MemoryRouter>
    );

    const assetLinks = screen.getAllByText("Link to USDM");
    expect(assetLinks[0]).toHaveAttribute(
      "href",
      "https://preprod.cexplorer.io/asset/asset1234567890"
    );

    const txLinks = screen.getAllByText("Link to blockchain");
    expect(txLinks[0]).toHaveAttribute(
      "href",
      "https://preprod.cardanoscan.io/transaction/hashabc123456"
    );
  });

  it("renders fallback label when filterValue is unrecognized", () => {
    render(
      <MemoryRouter>
        <DynamicTable transactions={mockTransactions} filterValue="Unknown" />
      </MemoryRouter>
    );

    expect(screen.getByText("Amount")).toBeInTheDocument();
    const unknownLinks = screen.getAllByText("Link to Unknown");
  });

  it("renders nothing when transactions list is empty", () => {
    render(
      <MemoryRouter>
        <DynamicTable transactions={[]} filterValue="USDM" />
      </MemoryRouter>
    );

    expect(screen.getByText("Transaction Date")).toBeInTheDocument();
    expect(screen.queryByText("Link to USDM")).not.toBeInTheDocument();
  });
});
