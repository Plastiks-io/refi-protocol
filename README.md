# ♻️ Blockchain Plastic Credit Backend

> 🛠️ Backend system for managing the lifecycle of **roadmaps** and **plastic credits** on the **Cardano blockchain** using smart contracts and Lucid SDK.

---

## 📚 Table of Contents

- [♻️ Blockchain Plastic Credit Backend](#️-blockchain-plastic-credit-backend)
  - [📚 Table of Contents](#-table-of-contents)
  - [📖 Overview](#-overview)
  - [📁 Directory Structure](#-directory-structure)
  - [🔧 Dependencies](#-dependencies)
  - [🔑 Environment Variables](#-environment-variables)
  - [🧠 Controllers and Responsibilities](#-controllers-and-responsibilities)
    - [📌 Roadmap Controller (`roadmap.controller.ts`)](#-roadmap-controller-roadmapcontrollerts)
    - [👤 User Controller (`user.controller.ts`)](#-user-controller-usercontrollerts)
  - [🗃️ Database Configuration](#️-database-configuration)
  - [📦 Sequelize Model: `CompletedRoadmap`](#-sequelize-model-completedroadmap)
  - [🌐 API Routes](#-api-routes)
    - [🛣️ Roadmap Router (`routes/roadmap.routes.ts`)](#️-roadmap-router-routesroadmaproutests)
    - [👤 User Router (`routes/user.routes.ts`)](#-user-router-routesuserroutests)
  - [🚀 Getting Started](#-getting-started)
  - [🧪 Test Endpoints (Postman/Curl)](#-test-endpoints-postmancurl)

---

## 📖 Overview

This backend manages roadmap-based funding tied to **plastic credit tokens**. It leverages **smart contracts**, **Lucid SDK**, and **Sequelize ORM** to facilitate:

- 🔄 Initializing and updating roadmap progress
- 💰 Releasing funds on roadmap completion
- 🔍 Querying transactions and on-chain history
- 📂 Managing active and completed roadmaps

---

## 📁 Directory Structure

```bash
   📦 backend
    └── 📁src
        └── 📁controllers
            └── roadmap.controller.ts
            └── user.controller.ts
        └── 📁db
            └── config.ts
        └── index.ts
        └── 📁models
            └── completedRoadmap.model.ts
        └── 📁Refi smart contract
            └── cabal.project
            └── LICENSE
            └── new-template.cabal
            └── 📁output
                └── refi.addr
                └── refi.json
            └── README.md
            └── 📁src
                └── Compiler.hs
                └── Validator.hs
            └── 📁test
                └── Spec.hs
        └── 📁routes
            └── roadmap.routes.ts
            └── user.routes.ts
        └── 📁types
            └── roadmap.types.ts
    └── .env
    └── .gitignore
    └── package-lock.json
    └── package.json
    └── README.md
    └── roadmap.txt
    └── tsconfig.json
```

## 🔧 Dependencies

- [`lucid-cardano`](https://github.com/Lucid-Creators/lucid) - Cardano SDK for smart contracts
- [`blockfrost`](https://blockfrost.io) - Cardano blockchain API
- `express` - HTTP server framework
- `sequelize` - ORM for PostgreSQL
- `dotenv` - For environment variable management
- `axios` - For API calls

---

## 🔑 Environment Variables

| Variable                                                  | Description                         |
| --------------------------------------------------------- | ----------------------------------- |
| `BLOCKFROST_URL`                                          | Base URL for Blockfrost API         |
| `BLOCKFROST_PROJECT_ID`                                   | API key for Blockfrost              |
| `ADMIN_SEED`                                              | Mnemonic for admin wallet           |
| `CBOR`                                                    | Serialized Plutus script            |
| `PLASTIK`                                                 | Asset ID for Plastik token          |
| `USDM`                                                    | Asset ID for USDM token             |
| `DEAD_WALLET_ADDRESS`                                     | Wallet address to burn tokens       |
| `PLASTIC_CREDIT_ASSET_ID`                                 | Asset ID for PC token (for queries) |
| `PC_WALLET`                                               | Mnemonic seed for PC wallet         |
| `PC_ASSET_ID`                                             | Asset ID for PC token               |
| `DB_HOST`, `DB_PORT`, `DB_USER`, `DB_PASSWORD`, `DB_NAME` | PostgreSQL credentials              |

---

## 🧠 Controllers and Responsibilities

### 📌 Roadmap Controller (`roadmap.controller.ts`)

Handles on-chain operations for plastic credit roadmaps:

1. **`initializeRoadmap`** - 🟢 Creates a new roadmap on-chain
2. **`updateRoadmap`** - 🔄 Updates roadmap progress and metrics
3. **`releaseFunds`** - 🏁 Releases ADA & tokens on completion
4. **`getAllRoadmaps`** - 📋 Returns active on-chain roadmaps
5. **`queryTransaction`** - 🔍 Get roadmap data from tx hash
6. **`queryAddressHistory`** - 🧾 Shows address transaction history
7. **`getAllCompletedRoadmaps`** - ✅ Fetch all completed roadmaps from DB

### 👤 User Controller (`user.controller.ts`)

Manages plastic credit transactions:

- **`initializeLucid`** - Initializes Lucid SDK
- **`sentPC`** - Sends plastic credits to a user-specified address

---

## 🗃️ Database Configuration

Located in `db/config.ts`, it sets up Sequelize for PostgreSQL.

```ts
const sequelize = new Sequelize({
  host: process.env.DB_HOST,
  port: parseInt(process.env.DB_PORT || "5432", 10),
  ...
});
```

- Logs only SQL errors
- PostgreSQL used as the SQL dialect
- Environment-driven config

---

## 📦 Sequelize Model: `CompletedRoadmap`

Defines schema for storing completed roadmap details.

| Field                | Type    | Description                    |
| -------------------- | ------- | ------------------------------ |
| `id`                 | UUID    | Primary Key                    |
| `preId`              | STRING  | Participant ID                 |
| `roadmapId`          | STRING  | Unique roadmap identifier      |
| `roadmapName`        | STRING  | Name of the roadmap            |
| `completion`         | INTEGER | Completion progress            |
| `totalPlastic`       | INTEGER | Plastic quantity involved      |
| `recoveredPlastic`   | INTEGER | Recovered plastic quantity     |
| `sentPlasticTokens`  | INTEGER | Tokens sent                    |
| `totalPlasticTokens` | INTEGER | Tokens generated               |
| `preAddress`         | STRING  | Blockchain participant address |

---

## 🌐 API Routes

### 🛣️ Roadmap Router (`routes/roadmap.routes.ts`)

| Method | Endpoint         | Description                          |
| ------ | ---------------- | ------------------------------------ |
| GET    | `/all`           | Fetch active roadmaps                |
| POST   | `/initialize`    | Initialize a new roadmap             |
| POST   | `/update`        | Update roadmap progress              |
| POST   | `/release`       | Release funds when complete          |
| POST   | `/query/txs`     | Query by transaction hash            |
| POST   | `/history/addr`  | Get address history                  |
| GET    | `/completed/all` | Get completed roadmaps from database |

### 👤 User Router (`routes/user.routes.ts`)

| Method | Endpoint   | Description                |
| ------ | ---------- | -------------------------- |
| POST   | `/send-pc` | Send plastic credit tokens |

---

## 🚀 Getting Started

1. Clone the repo:

   ```bash
   git clone https://github.com/AIQUANT-Tech/REFI-server.git
   cd REFI-server
   ```

2. Install dependencies:

   ```bash
   npm install
   ```

3. Setup `.env` file with all required variables.

4. Run the backend:
   ```bash
   npm run dev
   ```

## 🧪 Test Endpoints (Postman/Curl)

You can test the routes using Postman or tools like `curl`. Make sure you have a valid `.env` and your PostgreSQL and Blockfrost keys are active.

[Potman Collection](Plastiks.postman_collection.json)
