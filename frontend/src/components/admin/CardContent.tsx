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
      <p className="text-gray-800 text-lg mb-1">{title}</p>
      <p className="text-3xl font-bold text-black">{value}</p>
      {subtitle && <p className="text-sm text-gray-400">{subtitle}</p>}
      {progress !== undefined && (
        <div className="mt-2 bg-gray-200 rounded-full">
          <div
            className="bg-[#082FB9] h-2 rounded"
            style={{ width: `${progress}%` }}
            role="presentation"
          ></div>
        </div>
      )}
    </div>
  );
}
