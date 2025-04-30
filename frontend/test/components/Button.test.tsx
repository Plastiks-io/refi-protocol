import React from "react";
import { render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import Button from "../../src/components/Button";
import { faCheck } from "@fortawesome/free-solid-svg-icons";
import { vi } from "vitest";

describe("Button component", () => {
  it("renders with default variant and text", () => {
    render(<Button>Click Me</Button>);
    const button = screen.getByRole("button", { name: /click me/i });
    expect(button).toBeInTheDocument();
    expect(button).toHaveClass("bg-gray-800");
  });

  it("renders with outline variant", () => {
    render(<Button variant="outline">Outline</Button>);
    const button = screen.getByRole("button", { name: /outline/i });
    expect(button).toHaveClass("border", "text-gray-600");
  });

  it("renders with icon and text", () => {
    render(<Button icon={faCheck}>Save</Button>);
    const button = screen.getByRole("button", { name: /save/i });
    expect(button.querySelector("svg")).toBeInTheDocument();
  });

  it("supports onClick event", async () => {
    const user = userEvent.setup();
    const handleClick = vi.fn();

    render(<Button onClick={handleClick}>Press</Button>);
    const button = screen.getByRole("button", { name: /press/i });

    await user.click(button);
    expect(handleClick).toHaveBeenCalledTimes(1);
  });

  it("applies additional class names", () => {
    render(<Button className="custom-class">Extra</Button>);
    const button = screen.getByRole("button", { name: /extra/i });
    expect(button).toHaveClass("custom-class");
  });
});
