import React from "react";
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen } from "@testing-library/react";
import "@testing-library/jest-dom";
import { useSelector } from "react-redux";
import { MemoryRouter } from "react-router-dom";
import PrivateLendRoute from "../../src/components/PrivateLendRoute";

vi.mock("react-redux", async () => {
  const actual = await vi.importActual("react-redux");
  return {
    ...actual,
    useSelector: vi.fn(),
  };
});

describe("PrivateLendRoute Component", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("renders children when wallet address is present", () => {
    (useSelector as any).mockImplementation((selector: any) =>
      selector({
        wallet: {
          walletAddress: "addr1qxyzabc1234567890",
        },
      })
    );

    render(
      <MemoryRouter>
        <PrivateLendRoute>
          <div>Protected Content</div>
        </PrivateLendRoute>
      </MemoryRouter>
    );

    expect(screen.getByText("Protected Content")).toBeInTheDocument();
  });

  it("redirects to '/' when wallet address is not present", () => {
    (useSelector as any).mockImplementation((selector: any) =>
      selector({
        wallet: {
          walletAddress: null,
        },
      })
    );

    render(
      <MemoryRouter initialEntries={["/lend"]}>
        <PrivateLendRoute>
          <div>Protected Content</div>
        </PrivateLendRoute>
      </MemoryRouter>
    );

    expect(screen.queryByText("Protected Content")).not.toBeInTheDocument();
  });
});
