import dotenv from "dotenv";

// Determine environment
const NODE_ENV = process.env.NODE_ENV || "development";
console.log(`🌍 Loading environment: ${NODE_ENV}`);

// Load environment-specific configuration
dotenv.config({ path: `.env.${NODE_ENV}` });
dotenv.config({ path: ".env" }); // Fallback to base .env

export const config = {
  // Environment
  NODE_ENV,

  // Server Configuration
  PORT: Number(process.env.PORT) || 8080,

  // Frontend URLs
  FRONTEND_URL: process.env.FRONTEND_URL || "http://localhost:3000",
  FRONTEND_URL_2: process.env.FRONTEND_URL_2 || "http://localhost:3001",

  // Blockfrost Configuration
  BLOCKFROST: {
    URL:
      process.env.BLOCKFROST_URL ||
      "https://cardano-preprod.blockfrost.io/api/v0",
    PROJECT_ID: process.env.BLOCKFROST_PROJECT_ID,
  },

  // Asset Configuration
  ASSETS: {
    POLICY_ID: process.env.POLICY_ID,
    PLASTIC_TOKEN_NAME: process.env.PLASTIC_TOKEN_NAME,
    USDM_TOKEN_NAME: process.env.USDM_TOKEN_NAME,
  },

  // Wallet Configuration
  WALLETS: {
    ADMIN_SEED: process.env.ADMIN_SEED,
    ADMIN_PKH: process.env.ADMIN_PKH,
    NOZAMA_ADDRESS: process.env.NOZAMA_ADDRESS,
    PC_WALLET: process.env.PC_WALLET,
    DEAD_WALLET_ADDRESS: process.env.DEAD_WALLET_ADDRESS,
    ADMIN_WALLET_ADDRESS: process.env.ADMIN_WALLET_ADDRESS,
  },

  // Database Configuration
  DATABASE: {
    HOST: process.env.DB_HOST || "localhost",
    PORT: Number(process.env.DB_PORT) || 5432,
    USER: process.env.DB_USER,
    PASSWORD: process.env.DB_PASSWORD,
    NAME: process.env.DB_NAME,
  },

  // Super Admin Configuration
  SUPER_ADMIN: {
    EMAIL: process.env.email,
    PASSWORD: process.env.password,
    ADDRESS: process.env.address,
  },

  // Security
  JWT_SECRET: process.env.JWT_SECRET,

  // Smart Contracts
  CONTRACTS: {
    STAKE_REWARD_CBOR: process.env.STAKE_REWARD_CBOR,
    REFI_CBOR: process.env.REFI_CBOR,
    GOVERNANCE_CBOR: process.env.GOVERNANCE_CBOR,
  },

  REDIS: {
    host: process.env.REDIS_HOST || "127.0.0.1",
    port: parseInt(process.env.REDIS_PORT || "6379"),
  },

  JWT_COOKIE_NAME: process.env.JWT_COOKIE_NAME || "token",
} as const;

// Environment-specific validation
const getRequiredVarsForEnv = (env: string): string[] => {
  const baseRequired = ["JWT_SECRET", "DB_USER", "DB_PASSWORD", "DB_NAME"];

  switch (env) {
    case "production":
      return [
        ...baseRequired,
        "BLOCKFROST_PROJECT_ID",
        "POLICY_ID",
        "ADMIN_SEED", // Be careful with this in production
      ];
    case "testing":
      return [...baseRequired];
    case "development":
    default:
      return [...baseRequired, "BLOCKFROST_PROJECT_ID", "POLICY_ID"];
  }
};

// Validate required environment variables
const requiredVars = getRequiredVarsForEnv(NODE_ENV);
const missingVars: string[] = [];

requiredVars.forEach((varName) => {
  // Navigate nested config object to check if variable exists
  const keys = varName.split(".");
  let current: any = config;

  for (const key of keys) {
    if (current && typeof current === "object" && key in current) {
      current = current[key];
    } else {
      current = process.env[varName];
      break;
    }
  }

  if (!current) {
    missingVars.push(varName);
  }
});

if (missingVars.length > 0) {
  console.error(
    `❌ Missing required environment variables: ${missingVars.join(", ")}`
  );
  process.exit(1);
}

// Log configuration (without sensitive data)
console.log(`📋 Configuration loaded:`);
console.log(`   Environment: ${config.NODE_ENV}`);
console.log(`   Port: ${config.PORT}`);
console.log(
  `   Database: ${config.DATABASE.HOST}:${config.DATABASE.PORT}/${config.DATABASE.NAME}`
);
console.log(`   Blockfrost: ${config.BLOCKFROST.URL}`);
console.log(`   Frontend: ${config.FRONTEND_URL}`);

export default config;
