import { describe, it, expect, vi, beforeEach } from "vitest";
import { render } from "@testing-library/react";
import PrivateAdminRoute from "../../src/components/PrivateAdminRoute";
import React from "react";
import { useSelector } from "react-redux";
import { Navigate } from "react-router-dom";

// Mock useSelector and import.meta.env
vi.mock("react-redux", () => ({
  useSelector: vi.fn(),
}));

vi.mock("react-router-dom", async () => {
  const actual = await vi.importActual<any>("react-router-dom");
  return {
    ...actual,
    Navigate: ({ to }: { to: string }) => <div>Navigate to {to}</div>,
  };
});

describe("PrivateAdminRoute", () => {
  const OLD_ENV = { ...import.meta.env };

  beforeEach(() => {
    vi.clearAllMocks();
    Object.assign(import.meta, { env: { ...OLD_ENV } });
  });

  it("renders children if walletAddress matches admin address (case-insensitive)", () => {
    import.meta.env.VITE_ADMIN_WALLET_ADDRESS || "0xAdmin";
    (useSelector as any).mockImplementation((fn: any) =>
      fn({ wallet: { walletAddress: "0xadmin" } })
    );

    const { getByText } = render(
      <PrivateAdminRoute>
        <div>Admin Content</div>
      </PrivateAdminRoute>
    );
    expect(getByText("Navigate to /")).toBeTruthy();
  });

  it("navigates to / if walletAddress does not match admin address", () => {
    import.meta.env.VITE_ADMIN_WALLET_ADDRESS || "0xAdmin";
    (useSelector as any).mockImplementation((fn: any) =>
      fn({ wallet: { walletAddress: "0xUser" } })
    );

    const { getByText } = render(
      <PrivateAdminRoute>
        <div>Admin Content</div>
      </PrivateAdminRoute>
    );
    expect(getByText("Navigate to /")).toBeTruthy();
  });

  it("navigates to / if walletAddress is undefined", () => {
    import.meta.env.VITE_ADMIN_WALLET_ADDRESS || "0xAdmin";
    (useSelector as any).mockImplementation((fn: any) =>
      fn({ wallet: { walletAddress: undefined } })
    );

    const { getByText } = render(
      <PrivateAdminRoute>
        <div>Admin Content</div>
      </PrivateAdminRoute>
    );
    expect(getByText("Navigate to /")).toBeTruthy();
  });

  it("navigates to / if VITE_ADMIN_WALLET_ADDRESS is undefined", () => {
    import.meta.env.VITE_ADMIN_WALLET_ADDRESS || undefined;
    (useSelector as any).mockImplementation((fn: any) =>
      fn({ wallet: { walletAddress: "0xAdmin" } })
    );

    const { getByText } = render(
      <PrivateAdminRoute>
        <div>Admin Content</div>
      </PrivateAdminRoute>
    );
    expect(getByText("Navigate to /")).toBeTruthy();
  });

  it("navigates to / if both walletAddress and VITE_ADMIN_WALLET_ADDRESS are undefined", () => {
    import.meta.env.VITE_ADMIN_WALLET_ADDRESS || undefined;
    (useSelector as any).mockImplementation((fn: any) =>
      fn({ wallet: { walletAddress: undefined } })
    );

    const { getByText } = render(
      <PrivateAdminRoute>
        <div>Admin Content</div>
      </PrivateAdminRoute>
    );
    expect(getByText("Navigate to /")).toBeTruthy();
  });
});
