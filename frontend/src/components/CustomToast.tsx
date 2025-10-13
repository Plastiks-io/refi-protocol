import React from "react";
import { toast } from "sonner";
import { CircleCheck, ExternalLink, X } from "lucide-react";

interface ToastData {
  title: string;
  description: string;
  linkText?: string;
  linkUrl?: string;
}

// Custom toast component that matches your design
const CustomToast: React.FC<ToastData & { onDismiss: () => void }> = ({
  title,
  description,
  linkText,
  linkUrl,
  onDismiss,
}) => {
  return (
    <div className="bg-blue-100 border-2 border-blue-400 rounded-lg p-4 min-w-[320px] shadow-lg">
      <div className="flex items-start justify-between">
        <div className="flex items-start space-x-3">
          {/* Green checkmark icon */}
          <div className="flex-shrink-0 mt-0.5">
            <CircleCheck className="w-5 h-5 text-blue-600" />
          </div>

          <div className="flex-1">
            {/* Title */}
            <div className="text-black font-medium text-md mb-1">{title}</div>

            {/* Description */}
            <div className="text-gray-800 text-sm mb-2">{description}</div>

            {/* Link */}
            {linkText && (
              <div className="flex items-center space-x-1">
                <a
                  href={linkUrl || "#"}
                  className="text-sm text-blue-600 hover:underline"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  {linkText}
                </a>
                <ExternalLink className="w-3 h-3 text-blue-600" />
              </div>
            )}
          </div>
        </div>

        {/* Close button */}
        <button onClick={onDismiss} className="text-blue-600 hover:ml-2">
          <X className="w-4 h-4" />
        </button>
      </div>
    </div>
  );
};

// Hook to show the custom toast
export const useTransactionToast = () => {
  const showTransactionToast = (data: ToastData) => {
    toast.custom(
      (t) => <CustomToast {...data} onDismiss={() => toast.dismiss(t)} />,
      {
        duration: 5000, // 5 seconds
        position: "top-right",
      }
    );
  };

  return { showTransactionToast };
};
