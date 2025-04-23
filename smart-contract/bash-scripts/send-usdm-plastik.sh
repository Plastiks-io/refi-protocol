#!/bin/bash
# Define sender and receiver addresses


# Ensure sender and receiver addresses are set


# Export wallet and signing key for the sender

# source getTxFunc.sh
NETWORK="--testnet-magic 1"

echo "Entere receipient address"
read RECEIVE_ADDRESS
# RECEIVE_ADDRESS="addr_test1qpppxs0tpkfe0d4fdpwt4zp9qkw58ykk56lkegsae5wf8xws04ahatlde5v7z6ct8w49sr7q0rrazxy6d4qrrvk9rldsvx96t3"

echo -e "\n------------------------------------------------------------------"
echo "Select a UTxO with lovelace and tokens:"
echo "------------------------------------------------------------------"
chooseTokenUTXO $WALLET
TX_IN=${SELECTED_UTXO}
TOKEN_AMOUNT=${SELECTED_UTXO_TOKENS}
TOKEN_ASSET=${SELECTED_UTXO_ASSET}
LOVELACE_AMOUNT=${SELECTED_UTXO_LOVELACE}
echo "Selected UTxO: $TX_IN with $LOVELACE_AMOUNT lovelace and $TOKEN_AMOUNT $TOKEN_ASSET"
chooseWalletUTxO $WALLET
NEXT_TX_IN=${SELECTED_UTXO}
echo "Enter how much token you want to send"
read TOKEN_TO_SEND
# Calculate minimum required lovelace for token output
MIN_LOVELACE=$(cardano-cli conway transaction calculate-min-required-utxo \
    --protocol-params-file protocol.json \
    --tx-out "$RECEIVE_ADDRESS + 0 + $TOKEN_TO_SEND $TOKEN_ASSET" | awk '{print $2}')

# Add 10% buffer for safety
MIN_LOVELACE=$((MIN_LOVELACE * 110 / 100))
echo $MIN_LOVELACE

# Calculate remaining values
REMAINING_TOKENS=$((TOKEN_AMOUNT - TOKEN_TO_SEND))

# Create temporary files
TMP_RAW=$(mktemp)
TMP_SIG=$(mktemp)

# Build final transaction
cardano-cli conway transaction build \
    $NETWORK \
    --tx-in $TX_IN \
    --tx-in $NEXT_TX_IN \
    --tx-out "$RECEIVE_ADDRESS+$MIN_LOVELACE+ $TOKEN_TO_SEND $TOKEN_ASSET" \
    --tx-out "$ADDRESS + $MIN_LOVELACE + $REMAINING_TOKENS $TOKEN_ASSET" \
    --change-address $ADDRESS \
    --out-file $TMP_RAW

# Sign and submit
cardano-cli conway transaction sign \
    --tx-body-file $TMP_RAW \
    --signing-key-file $SKEY \
    $NETWORK \
    --out-file $TMP_SIG

cardano-cli conway transaction submit \
    $NETWORK \
    --tx-file $TMP_SIG

TX_ID=$(cardano-cli conway transaction txid --tx-file $TMP_SIG)
echo -e "\nTransaction successful with ID: $TX_ID"
echo "CardanoScan: https://preprod.cardanoscan.io/transaction/$TX_ID"

# Cleanup
rm -f $TMP_RAW $TMP_SIG