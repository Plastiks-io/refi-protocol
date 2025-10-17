// src/utils/helper.ts
import { Lucid, Blockfrost } from "lucid-cardano";

/**
 * Returns true if `addr` is a syntactically valid Cardano address (Shelley, Byron, CIP-19, etc.).
 * Internally it tries to parse it as Bech32; if it fails, returns false.
 */
export function isValidCardanoAddress(addr: string): boolean {
  if (!addr || typeof addr !== "string") return false;

  // Bech32 address pattern (Shelley era - starts with addr, addr_test, stake, etc.)
  const bech32Pattern = /^(addr|addr_test|stake|stake_test)1[a-zA-Z0-9]{50,}$/;

  // Byron era pattern (Base58 - starts with Ae2 for mainnet, 2c for testnet)
  const byronPattern =
    /^[123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz]{95,}$/;

  return bech32Pattern.test(addr) || byronPattern.test(addr);
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

// Define the TxSignError type
export interface TxSignError extends Error {
  code: number;
  info: string;
  name: "TxSignError";
}

// Type guard function
export function isTxSignError(error: unknown): error is TxSignError {
  return (
    error instanceof Error &&
    error.name === "TxSignError" &&
    "code" in error &&
    "info" in error
  );
}
