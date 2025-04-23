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

| Layer        | Technology                        |
|--------------|------------------------------------|
| Frontend     | React, TypeScript, Vite, Redux, Tailwind CSS |
| Backend      | Node.js, Express, TypeScript, PostgreSQL (optional) |
| Blockchain   | Cardano, Plutus Smart Contracts, Bash Scripts |
| Dev Tools    | Vite, ESlint, Prettier, Jest       |
| Infrastructure | GitHub Actions, .env configuration support |

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

| Feature | Preview |
|--------|---------|
| Dashboard | ![Dashboard](./screenshots/Screenshot%20from%202025-04-23%2016-44-27.png) |
| Roadmap Overview | ![Roadmaps](./screenshots/Screenshot%20from%202025-04-23%2016-44-42.png) |
| Wallet Selection | ![Wallets](./screenshots/Screenshot%20from%202025-04-23%2016-46-33.png) |
| Buy Plastik Credit | ![Voting](./screenshots/Screenshot%20from%202025-04-23%2016-47-44.png) |
| Admin Control | ![Admin](./screenshots/Screenshot%20from%202025-04-23%2016-47-23.png) |
| Wallet Payment | ![Buy Credit](./screenshots/Screenshot%20from%202025-04-23%2016-47-58.png) |
| Release Funds | ![Release](./screenshots/Screenshot%20from%202025-04-23%2016-48-31.png) |