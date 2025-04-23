# 🌍 Plastiks REFI Smart Contract ♻️

Welcome to the **Plastiks Smart Contract** project! This repository contains the implementation of a Plutus-based smart contract designed to facilitate secure and transparent transactions. Follow the instructions below to set up, compile, and test the project.

## 📋 Table of Contents

- [🌍 Plastiks REFI Smart Contract ♻️](#-plastiks-refi-smart-contract-️)
  - [📋 Table of Contents](#-table-of-contents)
  - [✅ Prerequisites](#-prerequisites)
  - [⚙️ Setup Instructions](#️-setup-instructions)
  - [🛠️ Compiling the Smart Contract](#️-compiling-the-smart-contract)
  - [🧪 Testing the Validator](#-testing-the-validator)
  - [📜 Transaction Onchain Commands](#-transaction-onchain-commands)
  - [📁 Project Structure](#-project-structure)
  - [📝 License](#-license)

## ✅ Prerequisites

Before you begin, ensure you have the following installed:

- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)
- [Cabal](https://www.haskell.org/cabal/)
- [Plutus Toolchain](https://github.com/input-output-hk/plutus)

## ⚙️ Setup Instructions

1. Clone the repository:

   ```bash
   git clone https://github.com/AIQUANT-Tech/REFI-Smart-Contracts
   cd REFI-Smart-Contracts
   ```

2. Update Cabal to download all required packages:
   ```bash
   cabal update
   ```

---

## 🛠️ Compiling the Smart Contract

1. Build the project:

   ```bash
   cabal build all
   ```

2. Start the REPL:

   ```bash
   cabal repl
   ```

3. Import the compiler module to compile the validator:

   ```haskell
   import Refi.Compiler
   ```

4. Run the function that outputs the CBOR file:
   ```haskell
   writeRefiScript
   ```
   This will generate the CBOR file inside the `output` directory.

## 🧪 Testing the Validator

To test the validator, run the following command:

```bash
cabal test
```

## 📜 Transaction Onchain Commands

All bash scripts related to building and submitting transactions can be found in the bash-scripts/ directory. These scripts are intended to help automate key operations on the Cardano blockchain using the compiled smart contract.

```bash
bash-scripts/lock.sh admin
bash-scripts/unlock.sh admin
```

## 📁 Project Structure

```
└── 📁smart-contract
    └── 📁bash-scripts
        └── 📁admin
            └── payment.addr
            └── payment.skey
            └── payment.vkey
            └── pubKey.hash
            └── stake.skey
            └── stake.vkey
        └── balance.sh
        └── command.txt
        └── create-preprod-wallet.sh
        └── 📁data
            └── datum-pre2.json
            └── datum-pre1.json
            └── description.txt
            └── redeemer-release.json
            └── redeemer-update-pre2.json
            └── redeemer-update-pre1.json
            └── updated-datum-pre2.json
            └── updated-datum-pre1.json
        └── generate-script-address.sh
        └── getTxFunc.sh
        └── lock-update.sh
        └── lock.sh
        └── 📁pre
            └── payment.addr
            └── payment.skey
            └── payment.vkey
            └── pubKey.hash
            └── stake.skey
            └── stake.vkey
        └── query-utxo.sh
        └── sendLovelacePreprod.sh
        └── 📁tmp
        └── tmp.txt
            └── utxos.txt
        └── unlock.sh
    └── 📁output
        └── refi.addr
        └── refi.json
    └── 📁src
        └── 📁Refi
            └── Compiler.hs
            └── Validator.hs
    └── 📁test
        └── Spec.hs
    └── .gitignore
    └── cabal.project
    └── new-template.cabal
    └── README.md
```

## 📝 License

This project is licensed under the [MIT License](LICENSE)

