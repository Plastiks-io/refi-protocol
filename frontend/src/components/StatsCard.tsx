import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import { IconProp } from "@fortawesome/fontawesome-svg-core";

type StatsCardProps = {
  title: string;
  value: string;
  description?: string;
  icon: IconProp;
  width: keyof typeof widthClasses;
};
const widthClasses = {
  "1/4": "lg:w-1/4",
  "1/3": "lg:w-1/3",
  "2/5": "lg:w-2/5",
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
      className={`bg-white p-6 text-black rounded-lg shadow-md flex flex-col items-start w-full ${widthClasses[width]}`}
    >
      {/* <div
      className={`bg-white p-6 text-black rounded-lg shadow-md flex flex-col items-start w-full lg:w-${width}`}
    > */}
      <div className="flex items-center justify-between w-full">
        <h3 className="text-gray-600 text-lg">{title}</h3>
        <FontAwesomeIcon icon={icon} size="lg" className="text-gray-400 ml-2" />
      </div>
      <h2 className="text-2xl sm:text-3xl font-bold">{value}</h2>
      {description && <p className="text-gray-500 text-sm">{description}</p>}
    </div>
  );
};

export default StatsCard;
