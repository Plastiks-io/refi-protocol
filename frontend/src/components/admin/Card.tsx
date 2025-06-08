// components/Card.jsx
import { ReactNode } from "react";

interface CardProps {
  children: ReactNode;
  className?: string;
}

export default function Card({ children, className = "" }: CardProps) {
  return (
    <div
      className={`bg-white rounded-2xl shadow-md border border-[#E5E7EB] p-4 ${className}`}
    >
      {children}
    </div>
  );
}
