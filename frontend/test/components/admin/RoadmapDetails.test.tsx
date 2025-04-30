import React from "react";
import { render, screen, fireEvent, waitFor } from "@testing-library/react";
import { Mock, vi } from "vitest";
import RoadmapDetails from "../../../src/components/admin/RoadmapDetails";
import { useSelector } from "react-redux";
import { useParams, useNavigate } from "react-router-dom";
import axios from "axios";
import { toast } from "sonner";

vi.mock("axios"); // Mock entire axios module
(axios.post as Mock).mockResolvedValue({ data: "Success" });

// Mock necessary hooks and modules
vi.mock("react-redux", () => ({
  useSelector: vi.fn(),
}));

vi.mock("react-router-dom", () => ({
  useParams: vi.fn(),
  useNavigate: vi.fn(),
}));

vi.mock("sonner", () => ({
  toast: {
    success: vi.fn(),
    error: vi.fn(),
  },
}));

describe("RoadmapDetails Component", () => {
  const mockNavigate = vi.fn();

  beforeEach(() => {
    vi.clearAllMocks();
    (useNavigate as any).mockReturnValue(mockNavigate);
  });

  it("renders loading state", () => {
    (useSelector as any).mockReturnValue({
      roadmaps: [],
      loading: true,
      error: null,
    });
    (useParams as any).mockReturnValue({ roadmapId: "123" });

    render(<RoadmapDetails />);
    expect(screen.getByText("Loading...")).toBeInTheDocument();
  });

  it("renders error state", () => {
    (useSelector as any).mockReturnValue({
      roadmaps: [],
      loading: false,
      error: "Error",
    });
    (useParams as any).mockReturnValue({ roadmapId: "123" });

    render(<RoadmapDetails />);
    expect(screen.getByText("Error loading roadmap")).toBeInTheDocument();
  });

  it("renders not found state", () => {
    (useSelector as any).mockReturnValue({
      roadmaps: [],
      loading: false,
      error: null,
    });
    (useParams as any).mockReturnValue({ roadmapId: "123" });

    render(<RoadmapDetails />);
    expect(screen.getByText("Roadmap not found")).toBeInTheDocument();
  });

  it("renders roadmap data correctly", () => {
    const mockRoadmap = {
      roadmapId: "123",
      roadmapName: "Sample Roadmap",
      progress: 75,
      recoveredPlastic: 300,
      sentPlasticTokens: 500,
      totalPlasticCredits: 1000,
      preId: "pre123",
    };

    (useSelector as any).mockReturnValue({
      roadmaps: [mockRoadmap],
      loading: false,
      error: null,
    });
    (useParams as any).mockReturnValue({ roadmapId: "123" });

    render(<RoadmapDetails />);

    expect(
      screen.getByText("Roadmap Details: Sample Roadmap")
    ).toBeInTheDocument();
    expect(screen.getByText("75%")).toBeInTheDocument();
    expect(screen.getByText("300 kg")).toBeInTheDocument();
    expect(screen.getByText("500 PLASTIK")).toBeInTheDocument();
    expect(screen.getByText("1000 USDM")).toBeInTheDocument();
  });

  it("displays error toast when releasing before 100% progress", async () => {
    const mockRoadmap = {
      roadmapId: "123",
      roadmapName: "Test",
      progress: 90,
      recoveredPlastic: 100,
      sentPlasticTokens: 100,
      totalPlasticCredits: 200,
      preId: "pre1",
    };

    (useSelector as any).mockReturnValue({
      roadmaps: [mockRoadmap],
      loading: false,
      error: null,
    });
    (useParams as any).mockReturnValue({ roadmapId: "123" });

    render(<RoadmapDetails />);
    const releaseBtn = screen.getByText("Release Funds");

    fireEvent.click(releaseBtn);
    expect(toast.error).toHaveBeenCalledWith(
      "Funds cannot be released until the progress is 100%"
    );
  });

  it("releases funds when progress is 100%", async () => {
    const mockRoadmap = {
      roadmapId: "123",
      roadmapName: "Test",
      progress: 100,
      recoveredPlastic: 100,
      sentPlasticTokens: 100,
      totalPlasticCredits: 200,
      preId: "pre1",
    };

    (useSelector as any).mockReturnValue({
      roadmaps: [mockRoadmap],
      loading: false,
      error: null,
    });
    (useParams as any).mockReturnValue({ roadmapId: "123" });
    (axios.post as any).mockResolvedValue({
      data: { message: "Funds released successfully" },
    });

    render(<RoadmapDetails />);
    const releaseBtn = screen.getByText("Release Funds");

    fireEvent.click(releaseBtn);

    await waitFor(() => {
      expect(toast.success).toHaveBeenCalledWith("Funds released successfully");
      expect(mockNavigate).toHaveBeenCalledWith("/admin");
    });
  });

  it("handles error when release API fails", async () => {
    const mockRoadmap = {
      roadmapId: "123",
      roadmapName: "Test",
      progress: 100,
      recoveredPlastic: 100,
      sentPlasticTokens: 100,
      totalPlasticCredits: 200,
      preId: "pre1",
    };

    (useSelector as any).mockReturnValue({
      roadmaps: [mockRoadmap],
      loading: false,
      error: null,
    });
    (useParams as any).mockReturnValue({ roadmapId: "123" });

    (axios.post as any).mockRejectedValue(new Error("Network error"));

    // Suppress console.error temporarily
    const originalError = console.error;
    console.error = vi.fn();

    render(<RoadmapDetails />);
    const releaseBtn = screen.getByText("Release Funds");

    fireEvent.click(releaseBtn);

    await waitFor(() => {
      expect(toast.error).toHaveBeenCalledWith(
        "Failed to release funds. Please try again."
      );
    });

    // Restore console.error
    console.error = originalError;
  });

});
