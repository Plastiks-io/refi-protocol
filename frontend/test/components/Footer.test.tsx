import React from "react";
import { render, screen } from "@testing-library/react";
import Footer from "../../src/components/Footer";
import { faTwitter, faDiscord, faGithub } from "@fortawesome/free-brands-svg-icons";
import { library } from "@fortawesome/fontawesome-svg-core";

// Add icons to the library for testing
library.add(faTwitter, faDiscord, faGithub);

describe("Footer", () => {
    it("renders the copyright text", () => {
        render(<Footer />);
        expect(
            screen.getByText("© 2025 Plastiks DApp. All rights reserved.")
        ).toBeInTheDocument();
    });

    it("renders three social media links", () => {
        render(<Footer />);
        const links = screen.getAllByRole("link");
        expect(links).toHaveLength(3);
    });

    it("renders the Twitter icon", () => {
        render(<Footer />);
        expect(screen.getByTestId("icon-twitter")).toBeInTheDocument();
    });

    it("renders the Discord icon", () => {
        render(<Footer />);
        expect(screen.getByTestId("icon-discord")).toBeInTheDocument();
    });

    it("renders the Github icon", () => {
        render(<Footer />);
        expect(screen.getByTestId("icon-github")).toBeInTheDocument();
    });
});

