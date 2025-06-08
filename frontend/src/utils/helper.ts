// src/utils/cardano.ts
import { Address } from "@emurgo/cardano-serialization-lib-browser";
import { Lucid, Blockfrost } from "lucid-cardano";

/**
 * Returns true if `addr` is a syntactically valid Cardano address (Shelley, Byron, CIP-19, etc.).
 * Internally it tries to parse it as Bech32; if it fails, returns false.
 */
export function isValidCardanoAddress(addr: string): boolean {
  try {
    // This will throw if the address is not valid Bech32 or Base58
    Address.from_bech32(addr);
    return true;
  } catch (e) {
    // It might also be a Byron‐era (Base58) address; try that too
    try {
      Address.from_bytes(Buffer.from(addr, "hex"));
      return true;
    } catch (_) {
      return false;
    }
  }
}

export async function initLucid() {
  try {
    const lucid = await Lucid.new(
      new Blockfrost(
        import.meta.env.VITE_BLOCKFROST_URL!,
        import.meta.env.VITE_BLOCKFROST_PROJECT_ID!
      ),
      "Preprod"
    );
    return lucid;
  } catch (error) {
    console.error("Error initializing Lucid:", error);
    throw new Error(
      "Failed to initialize Lucid. Please check your network connection."
    );
  }
}

export const decodeAddress = async (pkh: string, skh: string) => {
  try {
    const lucid = await initLucid();
    const paymentCredential = lucid.utils.keyHashToCredential(pkh);
    const stakeCredential = lucid.utils.keyHashToCredential(skh);
    const address = lucid.utils.credentialToAddress(
      paymentCredential,
      stakeCredential
    );

    return address;
  } catch (error) {
    console.error("Error decoding address:", error);
    throw new Error("Failed to decode address.");
  }
};

export const truncateAddress = (addr: string) => {
  if (addr.length <= 10) return addr;
  return `${addr.slice(0, 6)}…${addr.slice(-4)}`;
};

export const formatAmount = (amount: number) => {
  const roundedAmount = Math.round(amount * 100) / 100;
  return roundedAmount.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
};

/**
 * Accepts a Date object or ISO‐string, and returns an object containing:
 *  - datePart: formatted as "MMM DD, YYYY"
 *  - timePart: formatted as "h:mm AM/PM UTC"
 */
export const formatDateTime = (
  ts: Date | string
): { datePart: string; timePart: string } => {
  // 1. Ensure we have a JS Date object
  const dateObj = typeof ts === "string" ? new Date(ts) : ts;

  // 2. Define formatting options for the "datePart"
  //    month: "short"  → e.g. "Jan", "Feb", etc.
  //    day: "2-digit" → always two digits, e.g. "05", "15"
  //    year: "numeric" → full year like "2025"
  const dateOptions: Intl.DateTimeFormatOptions = {
    year: "numeric",
    month: "short",
    day: "2-digit",
  };

  // 3. Define formatting options for the "timePart"
  //    hour: "numeric"    → hour in 12‐hour format (no leading zero)
  //    minute: "2-digit"  → always two digits for minutes, e.g. "05", "30"
  //    hour12: true       → use 12‐hour clock (AM/PM)
  //    timeZone: "UTC"    → force output in UTC
  const timeOptions: Intl.DateTimeFormatOptions = {
    hour: "numeric",
    minute: "2-digit",
    hour12: true,
    timeZone: "UTC",
  };

  // 4. Format the date portion (e.g. "Jan 15, 2025")
  const datePart = new Intl.DateTimeFormat("en-US", dateOptions).format(
    dateObj
  );

  // 5. Format the time portion (e.g. "12:30 PM"), then append " UTC"
  const rawTime = new Intl.DateTimeFormat("en-US", timeOptions).format(dateObj);
  const timePart = `${rawTime} UTC`;

  return { datePart, timePart };
};
