type StatsCardProps = {
  title: string;
  value: string;
  description?: string;
  icon: string;
  width: keyof typeof widthClasses;
};
const widthClasses = {
  "1/4": "lg:w-1/4",
  "1/3": "lg:w-1/3",
  "2/5": "lg:w-2/5",
  "12/25": "lg:w-12/25",
};

const StatsCard = ({
  title,
  value,
  description,
  icon,
  width,
}: StatsCardProps) => {
  return (
    <div
      className={`bg-white p-6 text-black rounded-3xl flex flex-col items-start w-full border border-[#E5E7EB] ${widthClasses[width]}`}
    >
      <div className="flex items-center justify-between w-full">
        <h3 className="text-gray-800 text-xl">{title}</h3>
        <img src={icon} className="ml-2" />
      </div>
      <h2 className="text-2xl sm:text-3xl font-bold">{value}</h2>
      {description && <p className="text-gray-500 text-sm">{description}</p>}
    </div>
  );
};

export default StatsCard;
