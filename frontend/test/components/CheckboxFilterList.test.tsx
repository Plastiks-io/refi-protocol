import React from "react";
import { describe, it, expect, vi, beforeEach } from "vitest";
import { render, screen, fireEvent, cleanup } from "@testing-library/react";
import "@testing-library/jest-dom";
import CheckboxFilterList, {
  FilterOption,
} from "../../src/components/CheckboxFilterList";

const options: FilterOption[] = [
  { id: "a", label: "Option A" },
  { id: "b", label: "Option B" },
  { id: "c", label: "Option C" },
];

describe("CheckboxFilterList component", () => {
  beforeEach(() => {
    cleanup();
  });

  it("renders all options with correct labels", () => {
    render(<CheckboxFilterList options={options} />);
    options.forEach((opt) => {
      expect(screen.getByText(opt.label)).toBeInTheDocument();
    });
  });

  it("highlights initial selected option correctly", () => {
    render(<CheckboxFilterList options={options} initialSelected={["b"]} />);
    const button = screen.getAllByRole("button")[1]; // index 1 == 'b'
    expect(button).toHaveAttribute("aria-pressed", "true");
  });

  it("calls onChange with the selected ID when clicked", () => {
    const onChangeMock = vi.fn();
    render(
      <CheckboxFilterList
        options={options}
        initialSelected={["a"]}
        onChange={onChangeMock}
      />
    );

    fireEvent.click(screen.getByText("Option C"));
    expect(onChangeMock).toHaveBeenCalledWith(["c"]);
  });

  it("updates selected state when different option is clicked", () => {
    render(<CheckboxFilterList options={options} initialSelected={["a"]} />);

    const buttonA = screen
      .getByText("Option A")
      .closest("li")
      ?.querySelector("button");
    const buttonC = screen
      .getByText("Option C")
      .closest("li")
      ?.querySelector("button");

    expect(buttonA).toHaveAttribute("aria-pressed", "true");
    expect(buttonC).toHaveAttribute("aria-pressed", "false");

    fireEvent.click(screen.getByText("Option C"));

    expect(buttonA).toHaveAttribute("aria-pressed", "false");
    expect(buttonC).toHaveAttribute("aria-pressed", "true");
  });

  it("updates selected state when initialSelected prop changes", () => {
    const { rerender } = render(
      <CheckboxFilterList options={options} initialSelected={["a"]} />
    );

    expect(
      screen.getByText("Option A").closest("li")?.querySelector("button")
    ).toHaveAttribute("aria-pressed", "true");

    rerender(<CheckboxFilterList options={options} initialSelected={["b"]} />);

    expect(
      screen.getByText("Option B").closest("li")?.querySelector("button")
    ).toHaveAttribute("aria-pressed", "true");
  });

  it("renders check icon only for selected item", () => {
    render(<CheckboxFilterList options={options} initialSelected={["b"]} />);

    const checkIcons = screen.getAllByRole("img", { hidden: true });
    expect(checkIcons).toHaveLength(1);

    const selectedRow = screen.getByText("Option B").closest("li");
    expect(selectedRow?.querySelector("img")).toBeInTheDocument();
  });
});
