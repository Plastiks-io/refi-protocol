#!/usr/bin/env bash

set -euo pipefail

if [ "$#" -ne 2 ]; then
  echo "Usage: $0 <path-to-script> <path-to-output-address>"
  exit 1
fi

script=$1
output=$2

# Validate if the script file exists
if [[ ! -f "$script" ]]; then
    echo "Error: Script file '$script' does not exist."
    exit 1
fi

# Validate if the output directory is writable
output_dir=$(dirname "$output")
if [[ ! -d "$output_dir" || ! -w "$output_dir" ]]; then
    echo "Error: Output directory '$output_dir' does not exist or is not writable."
    exit 1
fi

# You can override TESTNET_MAGIC to use Mainnet or another Testnet
TESTNET_MAGIC=1

# Generate the Cardano script address
cardano-cli address build \
    --payment-script-file "$script" \
    --out-file "$output" \
    --testnet-magic "$TESTNET_MAGIC"

echo "Script address successfully generated and saved to '$output'."
