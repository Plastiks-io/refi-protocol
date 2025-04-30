import { render, screen } from "@testing-library/react";
import StatsCard from "../../src/components/StatsCard";
import { faCoffee } from "@fortawesome/free-solid-svg-icons";
import React from "react";

describe("StatsCard", () => {
    it("renders title and value", () => {
        render(
            <StatsCard
                title="Test Title"
                value="123"
                icon={faCoffee}
                width="1/4"
            />
        );
        expect(screen.getByText("Test Title")).toBeInTheDocument();
        expect(screen.getByText("123")).toBeInTheDocument();
    });

    it("renders description if provided", () => {
        render(
            <StatsCard
                title="Title"
                value="42"
                description="This is a description"
                icon={faCoffee}
                width="1/3"
            />
        );
        expect(screen.getByText("This is a description")).toBeInTheDocument();
    });

    it("does not render description if not provided", () => {
        render(
            <StatsCard
                title="Title"
                value="42"
                icon={faCoffee}
                width="2/5"
            />
        );
        expect(screen.queryByText("This is a description")).not.toBeInTheDocument();
    });

    it("applies correct width class for 1/4", () => {
        const { container } = render(
            <StatsCard
                title="Width Test"
                value="1"
                icon={faCoffee}
                width="1/4"
            />
        );
        expect(container.firstChild).toHaveClass("lg:w-1/4");
    });

    it("applies correct width class for 1/3", () => {
        const { container } = render(
            <StatsCard
                title="Width Test"
                value="2"
                icon={faCoffee}
                width="1/3"
            />
        );
        expect(container.firstChild).toHaveClass("lg:w-1/3");
    });

    it("applies correct width class for 2/5", () => {
        const { container } = render(
            <StatsCard
                title="Width Test"
                value="3"
                icon={faCoffee}
                width="2/5"
            />
        );
        expect(container.firstChild).toHaveClass("lg:w-2/5");
    });
});