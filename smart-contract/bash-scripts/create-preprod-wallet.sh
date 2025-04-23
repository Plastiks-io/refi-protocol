#!/bin/bash

walletName=$1
mkdir $walletName
cd $walletName

cardano-cli address key-gen \
    --verification-key-file payment.vkey \
    --signing-key-file payment.skey

cardano-cli conway stake-address key-gen \
    --verification-key-file stake.vkey \
    --signing-key-file stake.skey

cardano-cli conway address build \
    --payment-verification-key-file payment.vkey \
    --stake-verification-key-file stake.vkey \
    --out-file payment.addr \
    --testnet-magic 1

cardano-cli address key-hash --payment-verification-key-file payment.vkey --out-file pubKey.hash

pkh=$(cardano-cli address key-hash --payment-verification-key-file payment.vkey)
address=$(cat payment.addr)

