import StatsCard from "./StatsCard";
import { faDatabase, faFire, faLeaf } from "@fortawesome/free-solid-svg-icons";

const HeroSection = () => {
  const stats = [
    {
      title: "Total Tokens Retired",
      value: "234,567",
      description: "+2.5% from last month",
      icon: faFire,
    },
    {
      title: "Stablecoins Available",
      value: "$892,450",
      description: "USDC + USDM",
      icon: faDatabase,
    },
    {
      title: "Plastic Recovered",
      value: "45,678 kg",
      description: "Equivalent to x amount of bottles",
      icon: faLeaf,
    },
  ];
  return (
    <div className="bg-black">
      {/* Hero Section */}
      <section className="text-white text-center py-12 px-4">
        <div className="max-w-2xl mx-auto">
          <h1 className="text-3xl sm:text-4xl font-bold">
            Building a Sustainable Future with ReFi
          </h1>
          <p className="mt-4 text-base sm:text-lg">
            Plastiks ReFi Protocol ensures transparent and traceable funding for
            plastic recovery initiatives, empowering communities to combat
            plastic pollution.
          </p>
        </div>
        <button className="mt-6 px-8 sm:px-10 py-2 bg-white text-black font-semibold rounded-lg text-sm sm:text-base">
          CTA
        </button>
      </section>

      {/* Stats Section */}
      <div className="relative">
        {/* Black Background */}
        <div className="absolute inset-0 bg-black h-1/2"></div>
        {/* White Background */}
        <div className="absolute inset-0 top-1/2 bg-white h-1/2"></div>

        {/* Stats Cards */}
        <section className="relative flex flex-wrap justify-center gap-6 py-8 px-4">
          {stats.map((stat, index) => (
            <StatsCard
              key={index}
              title={stat.title}
              value={stat.value}
              description={stat.description}
              icon={stat.icon}
              width="1/4"
            />
          ))}
        </section>
      </div>
    </div>
  );
};

export default HeroSection;
