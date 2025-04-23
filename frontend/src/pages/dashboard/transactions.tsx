import DynamicTable from "../../components/Table";
import StatsCard from "../../components/StatsCard";
import { faFire, faLeaf } from "@fortawesome/free-solid-svg-icons";

const transactions = [
  {
    date: "01/01/2023",
    transactionFee: "2%",
    amount: "20 USD",
    tokenId: "2481",
    plasticCredit: "Link to plastic credit",
    hash: "Link to blockchain",
  },
  {
    date: "02/15/2023",
    transactionFee: "5%",
    amount: "125,000 USD",
    tokenId: "2481",
    plasticCredit: "Ocean Cleanup Initiative",
    hash: "Link to blockchain",
  },
  {
    date: "03/10/2023",
    transactionFee: "2%",
    amount: "125,000 USD",
    tokenId: "2481",
    plasticCredit: "Ocean Cleanup Initiative",
    hash: "Link to blockchain",
  },
  {
    date: "04/20/2023",
    transactionFee: "5%",
    amount: "125,000 USD",
    tokenId: "2481",
    plasticCredit: "Ocean Cleanup Initiative",
    hash: "Link to blockchain",
  },
  {
    date: "05/05/2023",
    transactionFee: "2%",
    amount: "125,000 USD",
    tokenId: "2481",
    plasticCredit: "Ocean Cleanup Initiative",
    hash: "Link to blockchain",
  },
  {
    date: "06/15/2023",
    transactionFee: "2%",
    amount: "125,000 USD",
    tokenId: "2481",
    plasticCredit: "Ocean Cleanup Initiative",
    hash: "Link to blockchain",
  },
];

const stats = [
  {
    title: "Total Tokens Retired",
    value: "234,567",
    description: "+2.5% from last month",
    icon: faFire,
  },
  {
    title: "Plastic Recovered",
    value: "45,678 kg",
    description: "Equivalent to x amount of bottles",
    icon: faLeaf,
  },
];

const Transactions = () => {
  return (
    <div className="bg-white text-gray-500 mx-auto px-4 md:px-10 lg:px-20 py-6">
      {/* Stats Cards */}
      <section className="relative flex flex-wrap justify-center gap-6 py-8 px-4">
        {stats.map((stat, index) => (
          <StatsCard
            key={index}
            title={stat.title}
            value={stat.value}
            description={stat.description}
            icon={stat.icon}
            width="2/5"
          />
        ))}
      </section>
      <h2 className="text-xl font-semibold text-gray-700">Transactions</h2>
      <p className="text-gray-500 mb-4">
        Below is a detailed list of transactions showcasing the fees, amounts,
        and associated plastic credits. This data highlights the impact and
        transparency of our plastic recovery initiatives.
      </p>
      <DynamicTable transactions={transactions} />
    </div>
  );
};

export default Transactions;
