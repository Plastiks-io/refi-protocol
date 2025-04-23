#!/bin/bash

if [ "$#" -ne 1 ]; then
  echo "Usage: lock-transaction (path-to-wallet-directory)"
  return
fi
WALLET=$1
source getTxFunc.sh
# Configuration
AMOUNT=1280070                   # Amount in Lovelace (1 ADA)
SENDER_ADDRESS=$(cat $WALLET/payment.addr)         # Your wallet address
echo "Sender Address: $SENDER_ADDRESS"
SENDER_SKEY="$WALLET/payment.skey"        # Your payment signing key
echo "Enter the script file path:"
read -r SCRIPT_FILE
# Validate if the script file exists
if [[ ! -f "$SCRIPT_FILE" ]]; then
    echo "Error: Script file '$SCRIPT_FILE' does not exist."
    exit 1
fi
echo "Enter script address file path:"
read -r SCRIPT_ADDRESS_FILE
# Validate if the script address file exists
if [[ ! -f "$SCRIPT_ADDRESS_FILE" ]]; then
    echo "Error: Script address file '$SCRIPT_ADDRESS_FILE' does not exist."
    exit 1
fi
SCRIPT_ADDRESS=$(cat $SCRIPT_ADDRESS_FILE) # Script address
echo "Script Address: $SCRIPT_ADDRESS"
echo "Enter the refi datum file path:"
read -r DATUM_FILE
# Validate if the escrow datum file exists
if [[ ! -f "$DATUM_FILE" ]]; then
    echo "Error: Escrow datum file '$DATUM_FILE' does not exist."
    exit 1
fi

chooseWalletUTxO $WALLET
TX_IN=${SELECTED_UTXO}

# Create temporary files
TMP_RAW=$(mktemp)
TMP_SIG=$(mktemp)
echo ""
echo "*******************************************************************************************"
echo "Building transaction..."
echo "*******************************************************************************************"
sleep 1
# Build and submit transaction
cardano-cli conway transaction build \
  --testnet-magic 1 \
  --tx-in $TX_IN \
  --tx-out "$SCRIPT_ADDRESS+$AMOUNT" \
  --tx-out-inline-datum-file $DATUM_FILE \
  --change-address $SENDER_ADDRESS \
  --out-file $TMP_RAW


echo "*******************************************************************************************"
echo "Signing transaction..."
echo "*******************************************************************************************"
sleep 1

cardano-cli conway transaction sign \
  --tx-body-file $TMP_RAW \
  --signing-key-file $SENDER_SKEY \
  --testnet-magic 1 \
  --out-file $TMP_SIG

echo "*******************************************************************************************"
echo "Submitting transaction..."
echo "*******************************************************************************************"
sleep 1

cardano-cli conway transaction submit \
  --tx-file $TMP_SIG \
  --testnet-magic 1

TX_ID=$(cardano-cli conway transaction txid --tx-file $TMP_SIG)
echo -e "\nTransaction successful with ID: $TX_ID"
echo "CardanoScan: https://preprod.cardanoscan.io/transaction/$TX_ID"

# Cleanup
rm -f $TMP_RAW $TMP_SIG