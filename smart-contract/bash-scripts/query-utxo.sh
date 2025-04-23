#!/bin/bash
if [ "$#" -ne 1 ]; then
    echo "Usage: query-utxo (path-to-address-file)"
    echo "Example: query-utxo /path/to/wallet/payment.addr"
  return
fi
address=$(< $1)

# Display the address being queried
echo "Fetching UTXOs for address: $address"

# Display UTXO Normally
cardano-cli query utxo --address "$address" --testnet-magic 1


# # Query UTXOs for the given address in JSON format
# output=$(cardano-cli query utxo --address "$address" --testnet-magic 1 --out-file /dev/stdout 2>&1)

# # Check if the command was successful
# if [[ $? -ne 0 ]]; then
#     echo "Error: Failed to fetch UTXOs. Details:"
#     echo "$output"
#     exit 1
# fi


# # Display the UTXO information in JSON format
# echo "UTXO information for address $address (in JSON):"
# echo "$output" | jq
