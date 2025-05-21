#!/bin/bash

# Enhanced script to generate a Cardano script address

# Prompt user for script file path
echo "Enter script file path:"
read -r script

# Validate if the script file exists
if [[ ! -f "$script" ]]; then
    echo "Error: Script file '$script' does not exist."
    exit 1
fi

# Prompt user for output file path
echo "Enter output file path:"
read -r output

# Validate if the output directory is writable
output_dir=$(dirname "$output")
if [[ ! -d "$output_dir" || ! -w "$output_dir" ]]; then
    echo "Error: Output directory '$output_dir' does not exist or is not writable."
    exit 1
fi

# Generate the Cardano script address
cardano-cli address build \
    --payment-script-file "$script" \
    --out-file "$output" \
    --testnet-magic 1

# Check if the command was successful
if [[ $? -eq 0 ]]; then
    echo "Script address successfully generated and saved to '$output'."
else
    echo "Error: Failed to generate script address."
    exit 1
fi
