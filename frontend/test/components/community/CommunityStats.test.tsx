import { render, screen } from "@testing-library/react";
import CommunityStats from "../../../src/components/community/CommunityStats";
import React from "react";

const stats = {
    currentRetirementRate: "10%",
    totalVotes: "100",
    totalPlastikVoted: "5000",
    remainingTime: "2 days",
};

describe("CommunityStats", () => {
    it("renders the title", () => {
        render(<CommunityStats stats={stats} votingAllowed={true} />);
        expect(screen.getByText("Current Statistics")).toBeInTheDocument();
    });

    it("renders all stats keys and values", () => {
        render(<CommunityStats stats={stats} votingAllowed={true} />);
        expect(screen.getByText("Current Retirement Rate")).toBeInTheDocument();
        expect(screen.getByText("10%")).toBeInTheDocument();
        expect(screen.getByText("Total Votes")).toBeInTheDocument();
        expect(screen.getByText("100")).toBeInTheDocument();
        expect(screen.getByText("Total Plastik Voted")).toBeInTheDocument();
        expect(screen.getByText("5000")).toBeInTheDocument();
        expect(screen.getByText("Remaining Time")).toBeInTheDocument();
        expect(screen.getByText("2 days")).toBeInTheDocument();
    });

    it("applies correct classes when votingAllowed is true", () => {
        render(<CommunityStats stats={stats} votingAllowed={true} />);
        const container = screen.getByText("Current Statistics").closest("div");
        expect(container).toHaveClass("bg-white");
        expect(container).not.toHaveClass("border");
    });

    it("applies correct classes when votingAllowed is false", () => {
        render(<CommunityStats stats={stats} votingAllowed={false} />);
        const container = screen.getByText("Current Statistics").closest("div");
        expect(container).toHaveClass("border");
    });

    it("renders stats in flex-col when votingAllowed is true", () => {
        render(<CommunityStats stats={stats} votingAllowed={true} />);
        const statsContainer = screen.getByText("Current Retirement Rate").parentElement?.parentElement;
        expect(statsContainer).toHaveClass("flex-col");
    });

    it("renders stats in flex-wrap when votingAllowed is false", () => {
        render(<CommunityStats stats={stats} votingAllowed={false} />);
        const statsContainer = screen.getByText("Current Retirement Rate").parentElement?.parentElement;
        expect(statsContainer).toHaveClass("flex-wrap");
    });
});