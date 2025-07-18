# ♻️ Plastiks.io — Plastic Recovery Platform

> A decentralized platform focused on accelerating **plastic recovery efforts** using **Cardano blockchain** technology. Built for transparency, accountability, and global impact.

## 📖 About the Project

**Plastiks.io** empowers individuals and organizations to recover plastic waste from the environment and verify the impact using blockchain. The platform integrates **smart contracts, escrow mechanisms, and community voting** to ensure traceability and accountability in plastic credit issuance.

This repository includes the full **frontend**, **backend**, and **smart contract** codebases needed to run the Plastiks dApp.

## 📚 Table of Contents

- [♻️ Plastiks.io — Plastic Recovery Platform](#️-plastiksio--plastic-recovery-platform)
  - [📖 About the Project](#-about-the-project)
  - [📚 Table of Contents](#-table-of-contents)
  - [✨ Key Features](#-key-features)
    - [🌍 Platform-Wide](#-platform-wide)
    - [🧑‍💻 Frontend](#-frontend)
    - [🔧 Backend](#-backend)
  - [🧱 Tech Stack](#-tech-stack)
  - [🗂️ Project Structure](#️-project-structure)
  - [🚀 Getting Started](#-getting-started)
    - [📦 Prerequisites](#-prerequisites)
  - [📸 Screenshots](#-screenshots)
  - [Use of USDM](#use-of-usdm)
    - [Why USDM is Used](#why-usdm-is-used)
      - [1. Plastik Token Volatility](#1-plastik-token-volatility)
      - [2. Stable Payments for Recovery Entities](#2-stable-payments-for-recovery-entities)
      - [3. Promoting Plastik Token Holding and Stability](#3-promoting-plastik-token-holding-and-stability)
    - [Where and How USDM is Used](#where-and-how-usdm-is-used)
    - [Admin Address](#admin-address)
    - [Summary](#summary)
    - [Release Fund Controller for Processing USDM Payments](#release-fund-controller-for-processing-usdm-payments)
    - [Test Case for Releasing Funds](#test-case-for-releasing-funds)
  - [Assets Used in Preprod Network](#assets-used-in-preprod-network)
  - [Transaction Hash to Verify that USDM is Sent to the Pre Wallet](#transaction-hash-to-verify-that-usdm-is-sent-to-the-pre-wallet)
- [Plastiks Lending Module](#plastiks-lending-module)
  - [📸 Screenshots](#-screenshots-1)

---

## ✨ Key Features

### 🌍 Platform-Wide

- ♻️ **Plastic Recovery Tracking**: Every action is recorded and verifiable.
- 💰 **Escrow-based Payments**: Fund release is conditional on milestones.
- 📊 **Dashboard and Analytics**: Visual insights into roadmap progress and recovery impact.
- 🗳️ **Community Voting**: Transparent governance over roadmap proposals.

### 🧑‍💻 Frontend

- Built with **React + TypeScript**
- Connects with **Cardano-compatible wallets**
- Dynamic views for **Admin**, **Users**, and **Community**
- Integrated with Redux for global state
- Responsive UI built with **Tailwind CSS**

### 🔧 Backend

- RESTful APIs with **Node.js + Express**
- Interacts with the Plutus-based smart contracts
- Secure token-based authentication
- Roadmap, voting, and transaction management
- Fully tested with **Jest**

---

## 🧱 Tech Stack

| Layer          | Technology                                          |
| -------------- | --------------------------------------------------- |
| Frontend       | React, TypeScript, Vite, Redux, Tailwind CSS        |
| Backend        | Node.js, Express, TypeScript, PostgreSQL (optional) |
| Blockchain     | Cardano, Plutus Smart Contracts, Bash Scripts       |
| Dev Tools      | Vite, ESlint, Prettier, Jest                        |
| Infrastructure | GitHub Actions, .env configuration support          |

---

## 🗂️ Project Structure

```bash
plastiks/
│
├── backend/               # Node.js API Server
│   ├── src/               # API logic (controllers, routes, DB)
│   ├── test/              # Jest tests
│   ├── .env               # Environment variables
│   └── README.md          # Backend documentation
│
├── frontend/              # React Frontend (User/Admin)
│   ├── src/               # React components & pages
│   ├── public/            # Static assets
│   ├── .env               # Frontend environment vars
│   └── README.md          # Frontend documentation
│
├── smart-contract/        # Plutus Smart Contracts
│   ├── src/               # Haskell contract logic
│   ├── bash-scripts/      # Interaction shell scripts
│   ├── output/            # Generated addresses and artifacts
│   └── README.md          # Smart contract deployment guide
│
└── README.md              # Root-level project documentation
```

> 📌 Each major directory contains its own `README.md` with specific setup and usage instructions.

---

## 🚀 Getting Started

### 📦 Prerequisites

- Node.js (v16+)
- npm or Yarn
- Cardano Node (for local testing of Plutus scripts)
- Git

---

## 📸 Screenshots

| Feature            | Preview                                                                    |
| ------------------ | -------------------------------------------------------------------------- |
| Dashboard          | ![Dashboard](./screenshots/Screenshot%20from%202025-04-23%2016-44-27.png)  |
| Roadmap Overview   | ![Roadmaps](./screenshots/Screenshot%20from%202025-04-23%2016-44-42.png)   |
| Wallet Selection   | ![Wallets](./screenshots/Screenshot%20from%202025-04-23%2016-46-33.png)    |
| Buy Plastik Credit | ![Voting](./screenshots/Screenshot%20from%202025-04-23%2016-47-44.png)     |
| Admin Control      | ![Admin](./screenshots/Screenshot%20from%202025-04-23%2016-47-23.png)      |
| Wallet Payment     | ![Buy Credit](./screenshots/Screenshot%20from%202025-04-23%2016-47-58.png) |
| Release Funds      | ![Release](./screenshots/Screenshot%20from%202025-04-23%2016-48-31.png)    |

## Use of USDM

In the **Plastik** project, the **USDM** stablecoin is used during fund releases to recovery entities.  
This design choice ensures stable, predictable payments and supports the long-term health of the Plastik token economy.

### Why USDM is Used

#### 1. Plastik Token Volatility

Plastik is a utility token, and like many such tokens, its price is subject to market fluctuations.  
Paying recovery entities directly in Plastik could lead to unpredictable real-world value at the time of payment.

#### 2. Stable Payments for Recovery Entities

By using **USDM** (a stablecoin pegged to the US Dollar), we guarantee that recovery entities receive the agreed-upon value without being affected by token price changes.

#### 3. Promoting Plastik Token Holding and Stability

Using USDM for payouts prevents unnecessary sell pressure on Plastik.  
This encourages holding, reduces volatility, and strengthens the overall Plastik ecosystem.

### Where and How USDM is Used

- **After Roadmap Milestone Completion**  
  Once a project milestone is successfully completed, an admin-controlled action is required to release funds.

- **Admin-Triggered Fund Release**  
  Only the designated Admin wallet address has permission to release funds after validating milestone completion.

- **USDM Transfer to Recovery Entity**  
  Upon approval, the contract releases the payment in **USDM** directly to the recovery entity’s wallet — not in Plastik tokens.

### Admin Address

```
addr_test1qregzqux7knjhg3v8npcp3t35w0dngwkz80ssgvywpk0ade9uy5qk6vl70ntchwh6qysnlww6q28vsjd6sz8kpdq2w0skcj8zp
```

### Summary

| Key Aspect         | Detail                                                                   |
| ------------------ | ------------------------------------------------------------------------ |
| **Payment Method** | USDM (Stablecoin)                                                        |
| **When**           | After Milestone Completion                                               |
| **Triggered By**   | Admin Address                                                            |
| **Benefits**       | Stable payments, reduced Plastik sell pressure, ecosystem stability      |
| **Test Coverage**  | Milestone completion, USDM transfer validation, Plastik token unaffected |

### Release Fund Controller for Processing USDM Payments

[Releasing Funds](./backend/src/controllers/roadmap.controller.ts#L344)

### Test Case for Releasing Funds

[Releasing Funds Test Section](./backend/test/roadmap.test.ts#L440)

## Assets Used in Preprod Network

The following asset IDs are used in the Preprod Cardano network:

- **PLASTIK TOKEN**  
  `d4fece6b39f7cd78a3f036b2ae6508c13524b863922da80f68dd9ab7504c415354494b`

- **USDM**  
  `d4fece6b39f7cd78a3f036b2ae6508c13524b863922da80f68dd9ab75553444d`

- **PLASTIK CREDIT**  
  `be0df58be0d65b9a944bea9488fc60d5320e5bea9b1ba4f5aece0ae1504c4153544943`

## Transaction Hash to Verify that USDM is Sent to the Pre Wallet

- **Hash**  
  `d52d27538d8faead63f35bada8859eac3eb979e939a370a66b446ed456a42d91`

- **Link**
  `https://preprod.cardanoscan.io/transaction/d52d27538d8faead63f35bada8859eac3eb979e939a370a66b446ed456a42d91`

# Plastiks Lending Module

## 📸 Screenshots

| Feature                                                                          | Preview                                                                          |
| -------------------------------------------------------------------------------- | -------------------------------------------------------------------------------- |
| Dashboard                                                                        | ![Dashboard](./screenshots/Screenshot%20from%202025-06-19%2010-00-23.png)        |
| Lend Tab                                                                         | ![Lend](./screenshots/Screenshot%20from%202025-06-19%2011-59-27.png)             |
| “Lend Plastik” Popup                                                             | ![Popup](./screenshots/Screenshot%20from%202025-06-19%2011-59-38.png)            |
| Plastik Payment                                                                  | ![Payment](./screenshots/Screenshot%20from%202025-06-19%2011-59-48.png)          |
| After Lending                                                                    | ![After Lend](./screenshots/Screenshot%20from%202025-06-19%2012-01-21.png)       |
| NFT Purchase                                                                     | ![Buy NFT](./screenshots/Screenshot%20from%202025-06-19%2012-02-17.png)          |
| Lender Receives Reward                                                           | ![Reward](./screenshots/Screenshot%20from%202025-06-19%2012-03-42.png)           |
| Withdraw Plastik                                                                 | ![Withdraw](./screenshots/Screenshot%20from%202025-06-19%2012-03-52.png)         |
| Withdraw Payment                                                                 | ![Withdraw Payment](./screenshots/Screenshot%20from%202025-06-19%2012-04-08.png) |
| 100% Withdrawal Also Claims Reward                                               | ![Full Withdraw](./screenshots/Screenshot%20from%202025-06-19%2012-04-25.png)    |
| “Redeem Rewards” Only                                                            | ![Redeem](./screenshots/Screenshot%20from%202025-06-19%2012-04-46.png)           |
| Admin: Missing Funds View                                                        | ![Missing Funds](./screenshots/Screenshot%20from%202025-06-19%2012-05-34.png)    |
| Admin Fund Popup                                                                 | ![Admin Fund Popup](./screenshots/Screenshot%20from%202025-06-19%2012-05-46.png) |
| Admin Payment Confirmation                                                       | ![Admin Payment](./screenshots/Screenshot%20from%202025-06-19%2012-06-05.png)    |
| USDM Funds Locked in ReFi Contract                                               | ![Funds Locked](./screenshots/Screenshot%20from%202025-06-19%2012-06-47.png)     |
| Once Roadmap Completes and funds locked in escrow released button will be active | ![Release Funds](./screenshots/Screenshot%20from%202025-06-21%2014-08-24.png)    |
| Admin Released funds popup                                                       | ![Admin Released](./screenshots/Screenshot%20from%202025-06-21%2014-09-55.png)   |
