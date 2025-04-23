import { faCircleCheck } from "@fortawesome/free-solid-svg-icons";
import Button from "../Button";

interface TokenBalanceProps {
  plastikBalance: number;
  votingAllowed: boolean;
}

const TokenBalance = ({ plastikBalance, votingAllowed }: TokenBalanceProps) => {
  return (
    <div className="bg-gray-100 p-4 rounded-lg mb-6 flex justify-between border border-gray-200">
      <div>
        <h2 className="text-lg font-semibold">Your Token Balance</h2>
        <p className="text-2xl font-bold">
          {new Intl.NumberFormat("en-US").format(plastikBalance)} Plastik
        </p>
      </div>
      <Button
        variant="userButton"
        className={
          votingAllowed
            ? "rounded-full bg-gray-900 text-white"
            : "rounded-full bg-gray-300 text-black"
        }
        icon={votingAllowed ? faCircleCheck : undefined}
      >
        {votingAllowed
          ? "Eligible to Start Voting Round"
          : "Not Eligible to Start Voting Round"}
      </Button>
    </div>
  );
};

export default TokenBalance;
