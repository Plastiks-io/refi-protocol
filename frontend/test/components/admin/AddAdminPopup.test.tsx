import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen, waitFor, fireEvent } from "@testing-library/react";
import AddAdminPopup from "../../../src/components/admin/AddAdminPopup";
import React from "react";
import "@testing-library/jest-dom"; // Import jest-dom matchers
// ===== Mock dependencies =====
vi.mock("axios");
vi.mock("sonner", () => ({
  toast: {
    success: vi.fn(),
    error: vi.fn(),
  },
}));
vi.mock("react-redux", () => ({
  useDispatch: () => vi.fn(),
}));
vi.mock("@/redux/adminSlice", () => ({
  addAdmin: vi.fn().mockReturnValue({ type: "ADD_ADMIN" }),
}));
vi.mock("@/utils/helper", () => ({
  isValidCardanoAddress: vi.fn(),
  truncateAddress: (addr: string) => `${addr.slice(0, 6)}...${addr.slice(-4)}`,
}));

import axios from "axios";
import { toast } from "sonner";
import { isValidCardanoAddress } from "../../../src/utils/helper";

// ===== Test setup =====
const mockOnClose = vi.fn();
const defaultProps = {
  isOpen: true,
  onClose: mockOnClose,
  existingAdmins: [
    { address: "addr1..." },
    { address: "addr2...", isCurrent: true },
  ],
  walletAddress: "addr2...",
};

describe("AddAdminPopup", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("should not render when isOpen is false", () => {
    render(<AddAdminPopup {...defaultProps} isOpen={false} />);
    expect(screen.queryByText("Add New Admin")).not.toBeInTheDocument();
  });

  it("renders the modal and its elements", () => {
    render(<AddAdminPopup {...defaultProps} />);
    expect(screen.getByText("Add New Admin")).toBeInTheDocument();
    expect(screen.getByPlaceholderText("addr…7b21")).toBeInTheDocument();
    expect(screen.getByText("Invite Admin")).toBeInTheDocument();
  });

  it("shows error toast for invalid Cardano address", async () => {
    (isValidCardanoAddress as any).mockReturnValue(false);

    render(<AddAdminPopup {...defaultProps} />);
    const input = screen.getByPlaceholderText("addr…7b21");
    const button = screen.getByText("Invite Admin");

    fireEvent.change(input, { target: { value: "invalid-address" } });
    fireEvent.click(button);

    await waitFor(() => {
      expect(toast.error).toHaveBeenCalledWith("Invalid Cardano address", {
        closeButton: true,
      });
    });
  });

  it("submits valid wallet address and shows success", async () => {
    (isValidCardanoAddress as any).mockReturnValue(true);
    (axios.post as any).mockResolvedValue({
      data: {
        admin: {
          address: "addr_test1234fsd34115431rfds43rwefsd43rewfads3421reqfwasd",
        },
      },
    });

    render(<AddAdminPopup {...defaultProps} />);
    const input = screen.getByPlaceholderText("addr…7b21");
    fireEvent.change(input, { target: { value: "addr1..." } });
    fireEvent.click(screen.getByText("Invite Admin"));

    await waitFor(() => {
      expect(axios.post).toHaveBeenCalled();
      expect(toast.success).toHaveBeenCalledWith("Admin added successfully", {
        closeButton: true,
      });
    });
  });

  it("shows error toast on API failure", async () => {
    (isValidCardanoAddress as any).mockReturnValue(true);
    (axios.post as any).mockRejectedValue(new Error("API error"));

    render(<AddAdminPopup {...defaultProps} />);
    const input = screen.getByPlaceholderText("addr…7b21");
    fireEvent.change(input, { target: { value: "addr1..." } });
    fireEvent.click(screen.getByText("Invite Admin"));

    await waitFor(() => {
      expect(toast.error).toHaveBeenCalledWith("Failed to add admin", {
        closeButton: true,
      });
    });
  });

  it("shows 'You' label for current wallet address", () => {
    render(<AddAdminPopup {...defaultProps} />);
    expect(screen.getByText("You")).toBeInTheDocument();
  });
});
