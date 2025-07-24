import React from "react";
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen } from "@testing-library/react";
import "@testing-library/jest-dom";
import { useSelector } from "react-redux";
import { MemoryRouter } from "react-router-dom";
import PrivateAdminRoute from "../../src/components/PrivateAdminRoute";

vi.mock("react-redux", async () => {
  const actual = await vi.importActual("react-redux");
  return {
    ...actual,
    useSelector: vi.fn(),
  };
});

describe("PrivateAdminRoute Component", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("renders children when role is ADMIN", () => {
    (useSelector as any).mockImplementation((selector: any) =>
      selector({ auth: { role: "ADMIN" } })
    );

    render(
      <MemoryRouter>
        <PrivateAdminRoute>
          <div>Admin Content</div>
        </PrivateAdminRoute>
      </MemoryRouter>
    );

    expect(screen.getByText("Admin Content")).toBeInTheDocument();
  });

  it("renders children when role is SUPER_ADMIN", () => {
    (useSelector as any).mockImplementation((selector: any) =>
      selector({ auth: { role: "SUPER_ADMIN" } })
    );

    render(
      <MemoryRouter>
        <PrivateAdminRoute>
          <div>Super Admin Content</div>
        </PrivateAdminRoute>
      </MemoryRouter>
    );

    expect(screen.getByText("Super Admin Content")).toBeInTheDocument();
  });

  it("redirects to '/' when role is not admin", () => {
    (useSelector as any).mockImplementation((selector: any) =>
      selector({ auth: { role: "USER" } })
    );

    render(
      <MemoryRouter initialEntries={["/admin"]}>
        <PrivateAdminRoute>
          <div>Should not render</div>
        </PrivateAdminRoute>
      </MemoryRouter>
    );

    expect(screen.queryByText("Should not render")).not.toBeInTheDocument();
  });

  it("redirects to '/' when role is null", () => {
    (useSelector as any).mockImplementation((selector: any) =>
      selector({ auth: { role: null } })
    );

    render(
      <MemoryRouter initialEntries={["/admin"]}>
        <PrivateAdminRoute>
          <div>Unauthorized</div>
        </PrivateAdminRoute>
      </MemoryRouter>
    );

    expect(screen.queryByText("Unauthorized")).not.toBeInTheDocument();
  });
});
