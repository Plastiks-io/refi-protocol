import React from "react";
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen, fireEvent } from "@testing-library/react";
import RoadmapRow from "../../src/components/RoadmapRow";
import { MemoryRouter } from "react-router-dom";

// Mock Button component
vi.mock("../../src/components/Button", () => ({
    default: ({ onClick, children }: any) => (
        <button onClick={onClick}>{children}</button>
    ),
}));

// Mock useNavigate
const mockNavigate = vi.fn();
vi.mock("react-router-dom", async (importOriginal) => {
    const actual = await importOriginal<any>();
    return {
        ...actual,
        useNavigate: () => mockNavigate,
    };
});

const defaultProps = {
    preId: "001",
    roadmapName: "Test Roadmap",
    roadmapDescription: "Test description",
    progress: 75,
    totalPlasticCredits: 12345,
    totalPlasticTokens: 6789,
    soldPlasticCredits: 1000,
    sentPlasticTokens: 500,
    totalPlastic: 2000,
    recoveredPlastic: 1500,
    roadmapId: "abc123",
};

describe("RoadmapRow", () => {
    beforeEach(() => {
        mockNavigate.mockClear();
    });

    it("renders all roadmap data correctly", () => {
        render(
            <table>
                <tbody>
                    <RoadmapRow {...defaultProps} />
                </tbody>
            </table>,
            { wrapper: MemoryRouter }
        );

        expect(screen.getByText("001")).toBeInTheDocument();
        expect(screen.getByText("Test Roadmap")).toBeInTheDocument();
        expect(screen.getByText(/USDM/)).toHaveTextContent("12,345 USDM");
        expect(screen.getByText(/PLASTIK/)).toHaveTextContent("6,789 PLASTIK");
        expect(screen.getByRole("button", { name: /Manage Roadmap/i })).toBeInTheDocument();
    });

    it("renders progress bar with correct width", () => {
        render(
            <table>
                <tbody>
                    <RoadmapRow {...defaultProps} />
                </tbody>
            </table>,
            { wrapper: MemoryRouter }
        );
        const progressBar = screen.getByRole("cell", { name: "" }).querySelector("div > div");
        expect(progressBar).toHaveStyle({ width: "75%" });
    });

    it("navigates to the correct route on button click", () => {
        render(
            <table>
                <tbody>
                    <RoadmapRow {...defaultProps} />
                </tbody>
            </table>,
            { wrapper: MemoryRouter }
        );
        fireEvent.click(screen.getByRole("button", { name: /Manage Roadmap/i }));
        expect(mockNavigate).toHaveBeenCalledWith("/admin/roadmap/abc123");
    });

    it("renders the current date in the correct format", () => {
        render(
            <table>
                <tbody>
                    <RoadmapRow {...defaultProps} />
                </tbody>
            </table>,
            { wrapper: MemoryRouter }
        );
        const today = new Date().toLocaleDateString("en-US", {
            year: "numeric",
            month: "short",
            day: "2-digit",
        });
        expect(screen.getByText(today)).toBeInTheDocument();
    });
});