import { useState, useEffect } from "react";
import { CheckIcon } from "@/assets/icons";

export type FilterOption = {
  id: string;
  label: string;
};

type CheckboxFilterListProps = {
  options: FilterOption[];
  initialSelected?: string[];
  onChange?: (selected: string[]) => void;
};

export default function CheckboxFilterList({
  options,
  initialSelected = [],
  onChange,
}: CheckboxFilterListProps) {
  const [selected, setSelected] = useState<string[]>(initialSelected);

  useEffect(() => {
    setSelected(initialSelected);
  }, [initialSelected]);

  function selectOnly(id: string) {
    const next = [id];
    setSelected(next);
    onChange?.(next);
  }

  return (
    <ul className="space-y-2 my-2">
      {options.map(({ id, label }) => {
        const isChecked = selected.includes(id);
        return (
          <li
            key={id}
            className="flex items-center gap-2 hover:bg-[#F4F4F4] px-2.5 py-0.5"
            onClick={() => selectOnly(id)}
          >
            <button
              // onClick={() => selectOnly(id)}
              className={`
                flex-shrink-0
                h-5 w-5 rounded-md border
                flex items-center justify-center
                transition-colors
                ${
                  isChecked
                    ? "bg-[#082FB9] border-blue-600"
                    : "bg-white border-gray-300"
                }
              `}
              aria-pressed={isChecked}
            >
              {isChecked && (
                <img
                  src={CheckIcon}
                  className="h-4 w-4 text-white"
                  aria-hidden
                />
              )}
            </button>
            <span className={`ml-2 text-lg select-none`}>{label}</span>
          </li>
        );
      })}
    </ul>
  );
}
