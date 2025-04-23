#!/bin/bash

if [ "$#" -ne 1 ]; then
  echo "Usage: tokenbalance.sh (path-to-wallet-directory)"
  return
fi
source getTxFunc.sh

WALLET=$1
ADDRESS=$(cat $WALLET/payment.addr)

# Query UTxOs for the address
utxo_output=$(cardano-cli query utxo \
  --address "$ADDRESS" \
  --testnet-magic 1)  # Replace `1` with the actual testnet magic number

# Display the UTxO output for reference
# echo "$utxo_output"

# Check if there are no UTxOs
if [[ $(echo "$utxo_output" | wc -l) -le 2 ]]; then
  echo "Total Native Balance: 0 ADA"
  echo "Plastik Token Balance: 0"
  echo "USDM Token Balance: 0"
  exit 0
fi

# Extract the lovelace amounts and calculate the total balance
total_lovelace=$(echo "$utxo_output" | awk 'NR>2 {sum+=$3} END {print sum}')  # Sum up the lovelace column
total_ada=$(echo "scale=6; $total_lovelace / 1000000" | bc)  # Convert to ADA with 6 decimal places

# Extract the balance of the plastik token
plastik_policy_id="e0b4a2454475355655a7449d3a064b38a22ba6fc83c637e2413ac172"
plastik_asset_name="504c415354494b"  # Hexadecimal representation of "PLASTIK"
plastik_balance=$(echo "$utxo_output" | awk -v policy="$plastik_policy_id" -v asset="$plastik_asset_name" '
  $0 ~ policy && $0 ~ asset {for (i=6; i<=NF; i++) if ($i ~ policy"."asset) print $(i-1)}
' | awk '{sum+=$1} END {print sum+0}')  # Sum up the token amounts

# Extract the balance of the USDM token
usdm_policy_id="e8f329da44fb66d033d26a566b0f9f743493b0cf18458c051ac44261"
usdm_asset_name="5553444d"  # Hexadecimal representation of "USDM"
usdm_balance=$(echo "$utxo_output" | awk -v policy="$usdm_policy_id" -v asset="$usdm_asset_name" '
  $0 ~ policy && $0 ~ asset {for (i=6; i<=NF; i++) if ($i ~ policy"."asset) print $(i-1)}
' | awk '{sum+=$1} END {print sum+0}')  # Sum up the token amounts

# Display the total balances
echo "Total Native Balance: $total_ada ADA"
echo "Plastik Token Available: $plastik_balance"
echo "USDM Token Available: $usdm_balance"
