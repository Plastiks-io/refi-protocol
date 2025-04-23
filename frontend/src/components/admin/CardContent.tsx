interface CardContentProps {
  title: string;
  value: string | number;
  subtitle?: string;
  children?: React.ReactNode;
  progress?: number;
}

export default function CardContent({
  title,
  value,
  subtitle,
  progress,
}: CardContentProps) {
  return (
    <div className="text-center py-4">
      <p className="text-gray-600 text-sm font-medium mb-1">{title}</p>
      <p className="text-xl font-bold text-black">{value}</p>
      {subtitle && <p className="text-sm text-gray-400">{subtitle}</p>}
      {progress !== undefined && (
        <div className="mt-2 bg-gray-200 rounded-full">
          <div
            className="bg-black h-2 rounded"
            style={{ width: `${progress}%` }}
          ></div>
        </div>
      )}
    </div>
  );
}
