#!/bin/bash

if [ "$#" -ne 1 ]; then
  echo "Usage: release-funds (path-to-wallet-directory)"
  return
fi
WALLET=$1
source getTxFunc.sh

SENDER_ADDRESS=$(cat $WALLET/payment.addr)
SENDER_SKEY="$WALLET/payment.skey"
PUBKEY_HASH=$(cat $WALLET/pubKey.hash)

echo "Sender Address: $SENDER_ADDRESS"

echo "Enter the script file path:"
read -r SCRIPT_FILE

echo "Enter script address file path:"
read -r SCRIPT_ADDRESS_FILE
SCRIPT_ADDRESS=$(cat $SCRIPT_ADDRESS_FILE)

echo "Enter the redeemer file path:"
read -r REDEEMER_FILE

chooseWalletUTxO $WALLET
TX_IN_FOR_FEE=${SELECTED_UTXO}

chooseContractUTxO $SCRIPT_ADDRESS
CONTRACT_UTXO=${SELECTED_UTXO}

TMP_RAW=$(mktemp)
TMP_SIG=$(mktemp)

echo ""
echo "*******************************************************************************************"
echo "Building transaction to release funds..."
echo "*******************************************************************************************"

# Query the value held in the contract UTXO (so we can forward it)
CONTRACT_VALUE=$(cardano-cli query utxo \
  --testnet-magic 1 \
  --address $SCRIPT_ADDRESS \
  --out-file /dev/stdout | jq -r 'to_entries[] | select(.key | contains("'"$CONTRACT_UTXO"'")) | .value.value.lovelace')

echo "Contract UTXO: $CONTRACT_UTXO"

# Build transaction
cardano-cli conway transaction build \
  --tx-in $CONTRACT_UTXO \
  --tx-in-script-file $SCRIPT_FILE \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $REDEEMER_FILE \
  --tx-in $TX_IN_FOR_FEE \
  --tx-in-collateral $TX_IN_FOR_FEE \
  --tx-out "$SENDER_ADDRESS+$CONTRACT_VALUE" \
  --change-address $SENDER_ADDRESS \
  --required-signer-hash $PUBKEY_HASH \
  --testnet-magic 1 \
  --out-file $TMP_RAW

echo "*******************************************************************************************"
echo "Signing transaction..."
echo "*******************************************************************************************"

cardano-cli conway transaction sign \
  --tx-body-file $TMP_RAW \
  --signing-key-file $SENDER_SKEY \
  --testnet-magic 1 \
  --out-file $TMP_SIG

echo "*******************************************************************************************"
echo "Submitting transaction..."
echo "*******************************************************************************************"

cardano-cli conway transaction submit \
  --tx-file $TMP_SIG \
  --testnet-magic 1

TX_ID=$(cardano-cli conway transaction txid --tx-file $TMP_SIG)
echo -e "\n🎉 Funds released successfully! Transaction ID: $TX_ID"
echo "CardanoScan: https://preprod.cardanoscan.io/transaction/$TX_ID"

# Cleanup
rm -f $TMP_RAW $TMP_SIG
