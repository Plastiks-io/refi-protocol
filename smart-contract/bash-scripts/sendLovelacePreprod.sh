#!/usr/bin/env bash

# source variables-private.sh
source getTxFunc.sh

WALLET=$1

NETWORK="--testnet-magic 1"
cardano-cli query tip $NETWORK

export ADDRESS=$(cat $WALLET/payment.addr)
export SKEY="$WALLET/payment.skey"

echo ""
echo "What is the recipient address?"
echo ""
read RECEIVE_ADDRESS

echo ""
echo "------------------------------------------------------------------"
echo "Select a UTxO with lovelace:"
echo "------------------------------------------------------------------"
echo ""
chooseWalletUTxO $WALLET
TX_IN=${SELECTED_UTXO}


echo "How many lovelace will you send?"
read LOVELACE_TO_SEND
# Create temporary files
TMP_RAW=$(mktemp)
TMP_SIG=$(mktemp)
# Correct placement of `--change-address` and `--out-file`
cardano-cli conway transaction build \
    $NETWORK \
    --tx-in $TX_IN \
    --tx-out $RECEIVE_ADDRESS+$LOVELACE_TO_SEND \
    --change-address $ADDRESS \
    --out-file $TMP_RAW

cardano-cli conway transaction sign \
    --tx-body-file $TMP_RAW \
    $NETWORK \
    --signing-key-file $SKEY \
    --out-file $TMP_SIG

cardano-cli conway transaction submit \
    $NETWORK \
    --tx-file $TMP_SIG

TX_ID=$(cardano-cli conway transaction txid --tx-file $TMP_SIG)
echo -e "\nTransaction successful with ID: $TX_ID"
echo "CardanoScan: https://preprod.cardanoscan.io/transaction/$TX_ID"

# Cleanup
rm -f $TMP_RAW $TMP_SIG
