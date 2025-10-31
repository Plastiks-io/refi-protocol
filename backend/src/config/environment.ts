// src/config/environment-refi.ts
import dotenv from "dotenv";
import { safeDecryptRefi } from "./decrypt.js";

const NODE_ENV = process.env.NODE_ENV || "development";
console.log(`🌍 Loading Refi environment: ${NODE_ENV}`);

dotenv.config({ path: `.env.${NODE_ENV}` });
dotenv.config({ path: ".env" });

/**
 * Helper to decrypt encrypted env values
 * Looks for KEY_ENCRYPTED in environment
 */
function getSecretValue(key: string): string | undefined {
  const encryptedKey = `${key}_ENCRYPTED`;
  const value = process.env[encryptedKey];

  if (!value) {
    return undefined;
  }

  return safeDecryptRefi(value);
}

export const refiConfig = {
  NODE_ENV,
  PORT: Number(process.env.PORT) || 8080,

  // Blockfrost
  BLOCKFROST: {
    URL:
      getSecretValue("BLOCKFROST_URL") ||
      "https://cardano-preprod.blockfrost.io/api/v0",
    PROJECT_ID: getSecretValue("BLOCKFROST_PROJECT_ID"),
  },

  // Assets
  ASSETS: {
    POLICY_ID: getSecretValue("POLICY_ID"),
    PLASTIC_TOKEN_NAME: getSecretValue("PLASTIC_TOKEN_NAME"),
    USDM_TOKEN_NAME: getSecretValue("USDM_TOKEN_NAME"),
  },

  // Wallet
  WALLETS: {
    ADMIN_SEED: getSecretValue("ADMIN_SEED"),
    ADMIN_PKH: getSecretValue("ADMIN_PKH"),
    NOZAMA_ADDRESS: getSecretValue("NOZAMA_ADDRESS"),
    PC_WALLET: getSecretValue("PC_WALLET"),
    DEAD_WALLET_ADDRESS: getSecretValue("DEAD_WALLET_ADDRESS"),
    ADMIN_WALLET_ADDRESS: getSecretValue("ADMIN_WALLET_ADDRESS"),
  },

  // Database
  DATABASE: {
    HOST: getSecretValue("DB_HOST") || "localhost",
    PORT: Number(getSecretValue("DB_PORT")) || 5432,
    USER: getSecretValue("DB_USER"),
    PASSWORD: getSecretValue("DB_PASSWORD"),
    NAME: getSecretValue("DB_NAME"),
  },

  // Super Admin
  SUPER_ADMIN: {
    EMAIL: getSecretValue("SUPER_ADMIN_EMAIL"),
    PASSWORD: getSecretValue("SUPER_ADMIN_PASSWORD"),
    ADDRESS: getSecretValue("SUPER_ADMIN_ADDRESS"),
  },

  // JWT
  JWT_SECRET: getSecretValue("JWT_SECRET"),

  // Frontend URLs
  FRONTEND_URL: process.env.FRONTEND_URL || "http://localhost:3000",
  FRONTEND_URL_2: process.env.FRONTEND_URL_2 || "http://localhost:3001",

  // Redis
  REDIS: {
    HOST: getSecretValue("REDIS_HOST") || "127.0.0.1",
    PORT: Number(getSecretValue("REDIS_PORT")) || 6379,
  },

  // CBOR
  CONTRACTS: {
    STAKE_REWARD_CBOR: getSecretValue("STAKE_REWARD_CBOR") || "cbor",
    REFI_CBOR: getSecretValue("REFI_CBOR") || "cbor",
    GOVERNANCE_CBOR: getSecretValue("GOVERNANCE_CBOR") || "cbor",
  },

  JWT_COOKIE_NAME: "token",
} as const;

// Validate required variables
const requiredVars = [
  "BLOCKFROST_PROJECT_ID",
  "ADMIN_SEED",
  "DB_USER",
  "DB_PASSWORD",
  "DB_NAME",
  "JWT_SECRET",
];

const missingVars: string[] = [];
requiredVars.forEach((varName) => {
  const value = getSecretValue(varName);
  if (!value) {
    missingVars.push(varName);
  }
});

if (missingVars.length > 0) {
  console.error(
    `❌ Missing required environment variables: ${missingVars.join(", ")}`
  );
  process.exit(1);
}

console.log(`📋 Refi Configuration loaded:`);
console.log(`   Environment: ${refiConfig.NODE_ENV}`);
console.log(`   Port: ${refiConfig.PORT}`);
console.log(
  `   Database: ${refiConfig.DATABASE.HOST}:${refiConfig.DATABASE.PORT}`
);
console.log(`   Blockfrost: ${refiConfig.BLOCKFROST.URL ? "✓" : "✗"}`);
console.log(
  `   Encryption: ${process.env.REFI_ENC ? "✓ Enabled" : "✗ Disabled"}`
);

export default refiConfig;
