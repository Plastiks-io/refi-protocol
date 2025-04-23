#!/usr/bin/env bash

if [ "$#" -ne 1 ]; then
  echo "Usage: send-all-utxos (path-to-wallet-directory)"
  exit 1
fi

source getTxFunc.sh

WALLET=$1
ADDRESS=$(cat "$WALLET/payment.addr")
SKEY="$WALLET/payment.skey"

selectAllUTXO "$WALLET"
UTXOS="${UTXO_LIST}"

# Create temporary files
TMP_RAW=$(mktemp)
TMP_SIG=$(mktemp)

# Build raw transaction to calculate fee
cardano-cli conway transaction build-raw \
    ${UTXOS} \
    --tx-out "$ADDRESS+${TOTAL_LOVELACE}" \
    --fee 0 \
    --out-file "$TMP_RAW"

FEE=$(cardano-cli conway transaction calculate-min-fee \
    --tx-body-file "$TMP_RAW" \
    --tx-in-count ${UTXO_COUNT} \
    --tx-out-count 1 \
    --testnet-magic 1 \
    --witness-count 1 \
    --protocol-params-file protocol.json | awk '{print $1}')

TOTAL_LOVELACE_TO_SEND=$((TOTAL_LOVELACE - FEE))

echo ""
echo "*******************************************************************************************"
echo "Building transaction..."
echo "*******************************************************************************************"
sleep 1
# Build final raw transaction with correct fee
cardano-cli conway transaction build-raw \
    ${UTXOS} \
    --tx-out "$ADDRESS+$TOTAL_LOVELACE_TO_SEND" \
    --fee "$FEE" \
    --out-file "$TMP_RAW"

echo "*******************************************************************************************"
echo "Signing transaction..."
echo "*******************************************************************************************"
sleep 1

# Sign the transaction
cardano-cli conway transaction sign \
    --signing-key-file "$SKEY" \
    --testnet-magic 1 \
    --tx-body-file "$TMP_RAW" \
    --out-file "$TMP_SIG"

echo "*******************************************************************************************"
echo "Submitting transaction..."
echo "*******************************************************************************************"
sleep 1

# Submit the transaction
cardano-cli conway transaction submit \
    --testnet-magic 1 \
    --tx-file "$TMP_SIG"

TX_ID=$(cardano-cli conway transaction txid --tx-file "$TMP_SIG")
echo "Transaction successful with ID: $TX_ID"
echo "CardanoScan: https://preprod.cardanoscan.io/transaction/$TX_ID"

# Cleanup
rm -f "$TMP_RAW" "$TMP_SIG"