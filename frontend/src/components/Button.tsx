import { cn } from "../lib/utils";
import { ButtonHTMLAttributes } from "react";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import { IconProp } from "@fortawesome/fontawesome-svg-core";

interface ButtonProps extends ButtonHTMLAttributes<HTMLButtonElement> {
  variant?: "default" | "outline" | "dark" | "gray" | "userButton";
  icon?: IconProp; // Make icon optional for text-only buttons
}

const Button: React.FC<ButtonProps> = ({
  variant = "default",
  className,
  children,
  icon,
  ...props
}) => {
  return (
    <button
      className={cn(
        "space-x-2 flex items-center justify-center px-6 py-2 transition cursor-pointer",
        variant === "default" &&
          "bg-gray-800 text-white hover:bg-gray-600 rounded-lg",
        variant === "outline" &&
          "flex items-center justify-center bg-white border border-[#0D0D0D] text-[#0D0D0D] px-4 py-2 rounded-full",
        variant === "dark" &&
          "bg-gray-900 text-white hover:bg-gray-700 font-semibold rounded-lg",
        variant === "gray" &&
          "bg-gray-200 text-base text-gray-800 hover:bg-gray-300 font-semibold rounded-lg",
        variant === "userButton",
        className
      )}
      {...props}
    >
      {icon && <FontAwesomeIcon icon={icon} size="lg" />}
      <span>{children}</span>
    </button>
  );
};

export default Button;
