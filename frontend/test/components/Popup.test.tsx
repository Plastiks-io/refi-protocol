import React from "react";
import { render, screen, fireEvent } from "@testing-library/react";
import Popup from "../../src/components/Popup";
import { vi } from "vitest";

const wallets = [
  {
    icon: "icon1.png",
    id: "wallet1",
    name: "Wallet One",
    version: "1.0.0",
  },
  {
    icon: "icon2.png",
    id: "wallet2",
    name: "Wallet Two",
    version: "2.1.0",
  },
];

describe("Popup", () => {
  it("renders the popup with title and wallets", () => {
    render(
      <Popup wallets={wallets} onSelectWallet={vi.fn()} onClose={vi.fn()} />
    );
    expect(screen.getByText("Select a Wallet")).toBeInTheDocument();
    expect(screen.getByText("Wallet One")).toBeInTheDocument();
    expect(screen.getByText("Wallet Two")).toBeInTheDocument();
    expect(screen.getByText("v1.0.0")).toBeInTheDocument();
    expect(screen.getByText("v2.1.0")).toBeInTheDocument();
    expect(screen.getByRole("button", { name: /close/i })).toBeInTheDocument();
  });

  it("calls onSelectWallet with correct id when a wallet is clicked", () => {
    const onSelectWallet = vi.fn();
    render(
      <Popup
        wallets={wallets}
        onSelectWallet={onSelectWallet}
        onClose={vi.fn()}
      />
    );
    fireEvent.click(screen.getByText("Wallet Two"));
    expect(onSelectWallet).toHaveBeenCalledWith("wallet2");
  });

  it("calls onClose when the close button is clicked", () => {
    const onClose = vi.fn();
    render(
      <Popup wallets={wallets} onSelectWallet={vi.fn()} onClose={onClose} />
    );
    fireEvent.click(screen.getByRole("button", { name: /close/i }));
    expect(onClose).toHaveBeenCalled();
  });

  it("renders no wallets if wallets array is empty", () => {
    render(<Popup wallets={[]} onSelectWallet={vi.fn()} onClose={vi.fn()} />);
    expect(screen.queryByText("Wallet One")).not.toBeInTheDocument();
    expect(screen.queryByText("Wallet Two")).not.toBeInTheDocument();
  });

  it("renders wallet icons with correct src and alt", () => {
    render(
      <Popup wallets={wallets} onSelectWallet={vi.fn()} onClose={vi.fn()} />
    );
    const images = screen.getAllByRole("img");
    expect(images[0]).toHaveAttribute("src", "icon1.png");
    expect(images[0]).toHaveAttribute("alt", "Wallet One");
    expect(images[1]).toHaveAttribute("src", "icon2.png");
    expect(images[1]).toHaveAttribute("alt", "Wallet Two");
  });
});
