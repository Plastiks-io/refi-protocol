# Usage: chooseWalletUTxO <path to wallet>
# Where wallet includes the following files:
# payment.addr
# payment.skey
# payment.vkey
# pubKey.hash
# stake.skey
# stake.vkey

# Wallets with this structure are built with /ppbl2023-plutus-template/bash-scripts/createPreprodWallet.sh

function chooseWalletUTxO() {
	rm -f -r tmp
	mkdir tmp
	touch tmp/utxos.txt
	UTXOS_FILE="./tmp/utxos.txt"
	if [ -z "$1" ]
	then
		read -p "Enter Wallet Directory: " WALLET_DIRECTORY
	else
		WALLET_DIRECTORY=$1
	fi
	WALLET_ADDRESS=$(cat $WALLET_DIRECTORY/payment.addr)
	WALLET_KEY="$WALLET_DIRECTORY/payment.skey"
	./balance.sh $WALLET_ADDRESS > $UTXOS_FILE
	echo $WALLET_ADDRESS

	tail -n +3 $UTXOS_FILE > tmp.txt

	n=1
	while read -r line
	do
	  echo "$n: $line"
	  n=$((n+1))
	done < tmp.txt

	echo ""
	read -p 'TX row number: ' TMP
	TX_ROW_NUM="$(($TMP+2))"
	TX_ROW=$(sed "${TX_ROW_NUM}q;d" $UTXOS_FILE)
	SELECTED_UTXO="$(echo $TX_ROW | awk '{ print $1 }')#$(echo $TX_ROW | awk '{ print $2 }')"
	SELECTED_UTXO_LOVELACE=$(echo $TX_ROW | awk '{ print $3 }')
	SELECTED_UTXO_TOKENS=$(echo $TX_ROW | awk '{ print $6 }')
	SELECTED_UTXO_ASSET=$(echo $TX_ROW | awk '{ print $7 }')
	# echo "Ok, you selected $SELECTED_UTXO"
	# echo "With $SELECTED_UTXO_LOVELACE lovelace"
	# echo "And $SELECTED_UTXO_TOKENS $SELECTED_UTXO_ASSET"
}

function chooseWalletUTxOWithoutToken() {
    rm -rf tmp
    mkdir -p tmp
    UTXOS_FILE="tmp/utxos.txt"

    if [ -z "$1" ]; then
        read -p "Enter Wallet Directory: " WALLET_DIRECTORY
    else
        WALLET_DIRECTORY=$1
    fi

    WALLET_ADDRESS=$(cat "$WALLET_DIRECTORY/payment.addr")
    WALLET_KEY="$WALLET_DIRECTORY/payment.skey"
    ./query-utxo.sh "$WALLET_DIRECTORY/payment.addr" > "$UTXOS_FILE"

    # Keep only UTxOs with a single '+' (tokenless)
    tail -n +3 "$UTXOS_FILE" | awk 'gsub(/\+/, "+") == 1' > tmp/tmp.txt

    if [ ! -s tmp/tmp.txt ]; then
        echo "❌ No UTxOs without tokens found in wallet $WALLET_ADDRESS."
        return 1
    fi

    echo "🔍 Select UTxOs without tokens for fee:"
    n=1
    while read -r line; do
        echo "$n: $line"
        n=$((n+1))
    done < tmp/tmp.txt

    echo ""
    read -p 'TX row number: ' TMP
    TX_ROW=$(sed "${TMP}q;d" tmp/tmp.txt)

    SELECTED_UTXO="$(echo "$TX_ROW" | awk '{ print $1 }')#$(echo "$TX_ROW" | awk '{ print $2 }')"
    SELECTED_UTXO_LOVELACE=$(echo "$TX_ROW" | awk '{ print $3 }')

    echo "✅ Selected UTxO: $SELECTED_UTXO"
    echo "💰 Lovelace: $SELECTED_UTXO_LOVELACE"
}


function chooseContractUTxO() {
	rm -f -r tmp
	mkdir tmp
	touch tmp/utxos.txt
	UTXOS_FILE="./tmp/utxos.txt"
	if [ -z "$1" ]
	then
		read -p "Enter Contract Address: " cADDRESS
	else
		cADDRESS=$1
	fi
	./balance.sh $cADDRESS > $UTXOS_FILE
	echo $cADDRESS

	tail -n +3 $UTXOS_FILE > tmp.txt

	n=1
	while read -r line
	do
	  echo "$n: $line"
	  n=$((n+1))
	done < tmp.txt

	read -p 'TX row number: ' TMP
	TX_ROW_NUM="$(($TMP+2))"
	TX_ROW=$(sed "${TX_ROW_NUM}q;d" $UTXOS_FILE)
	SELECTED_UTXO="$(echo $TX_ROW | awk '{ print $1 }')#$(echo $TX_ROW | awk '{ print $2 }')"
	SELECTED_UTXO_LOVELACE=$(echo $TX_ROW | awk '{ print $3 }')
	SELECTED_UTXO_TOKENS=$(echo $TX_ROW | awk '{ print $6 }')
	SELECTED_UTXO_ASSET=$(echo $TX_ROW | awk '{ print $7 }')
	# echo "Ok, you selected $SELECTED_UTXO"
	# echo "With $SELECTED_UTXO_LOVELACE lovelace"
	# echo "And $SELECTED_UTXO_TOKENS $SELECTED_UTXO_ASSET"
}

function chooseTokenUTXO(){
	rm -f -r tmp
	mkdir tmp
	touch tmp/utxos.txt
	UTXOS_FILE="./tmp/utxos.txt"

	if [ -z "$1" ]
	then
		read -p "Enter Wallet Directory: " WALLET_DIRECTORY
	else
		WALLET_DIRECTORY=$1
	fi

	WALLET_ADDRESS=$(cat $WALLET_DIRECTORY/payment.addr)
	WALLET_KEY="$WALLET_DIRECTORY/payment.skey"

	# Get balance (UTxOs)
	./balance.sh $WALLET_ADDRESS > $UTXOS_FILE

	# Ask user for token name
	read -p "Enter the token name (exact match required): " TOKEN_NAME
	TOKEN_HEXSTRING=$(xxd -pu <<<"$TOKEN_NAME")
	TOKEN_HEX=${TOKEN_HEXSTRING::-2}

	# Filter UTxOs that contain the specified token
	echo "Searching for UTxOs containing token: $TOKEN_NAME ($TOKEN_HEX)"
	grep "$TOKEN_HEX" $UTXOS_FILE > tmp/tmp.txt

	if [ ! -s tmp.txt ]; then
		echo "❌ No UTxOs found containing token: $TOKEN_NAME"
		return 1
	fi

	# Display filtered UTxOs
	n=1
	while read -r line
	do
	  echo "$n: $line"
	  n=$((n+1))
	done < tmp.txt

	echo ""
	read -p 'TX row number: ' TMP
	TX_ROW_NUM="$(($TMP))"
	TX_ROW=$(sed "${TX_ROW_NUM}q;d" tmp.txt)

	# Extract UTxO details
	SELECTED_UTXO="$(echo $TX_ROW | awk '{ print $1 }')#$(echo $TX_ROW | awk '{ print $2 }')"
	SELECTED_UTXO_LOVELACE=$(echo $TX_ROW | awk '{ print $3 }')
	SELECTED_UTXO_TOKENS=$(echo $TX_ROW | awk '{ print $6 }')
	SELECTED_UTXO_ASSET=$(echo $TX_ROW | awk '{ print $7 }')

	echo "✅ Selected UTxO: $SELECTED_UTXO"
	echo "💰 Lovelace: $SELECTED_UTXO_LOVELACE"
	echo "🪙 Token: $SELECTED_UTXO_TOKENS $SELECTED_UTXO_ASSET"
}

function chooseTokenUTXOWithName() {
    rm -f -r tmp
    mkdir -p tmp
    touch tmp/utxos.txt
    UTXOS_FILE="./tmp/utxos.txt"

    if [ -z "$1" ]; then
        read -p "Enter Wallet Directory: " WALLET_DIRECTORY
    else
        WALLET_DIRECTORY=$1
    fi

    if [ -z "$2" ]; then
        read -p "Enter Token Name (exact match required): " TOKEN_NAME
    else
        TOKEN_NAME=$2
    fi

    WALLET_ADDRESS=$(cat "$WALLET_DIRECTORY/payment.addr")
    WALLET_KEY="$WALLET_DIRECTORY/payment.skey"

    ./balance.sh "$WALLET_ADDRESS" > "$UTXOS_FILE"

    TOKEN_HEXSTRING=$(xxd -pu <<<"$TOKEN_NAME")
    TOKEN_HEX=${TOKEN_HEXSTRING::-2}

    grep "$TOKEN_HEX" "$UTXOS_FILE" > tmp/tmp.txt

    if [ ! -s tmp/tmp.txt ]; then
        echo "❌ No UTxOs found containing token: $TOKEN_NAME"
        return 1
    fi

    TX_ROW=$(head -n 1 tmp/tmp.txt)

    SELECTED_UTXO="$(echo "$TX_ROW" | awk '{ print $1 }')#$(echo "$TX_ROW" | awk '{ print $2 }')"
    SELECTED_UTXO_LOVELACE=$(echo "$TX_ROW" | awk '{ print $3 }')
    SELECTED_UTXO_TOKENS=$(echo "$TX_ROW" | awk '{ print $6 }')
    SELECTED_UTXO_ASSET=$(echo "$TX_ROW" | awk '{ print $7 }')

    # echo "✅ Selected UTxO: $SELECTED_UTXO"
    # echo "💰 Lovelace: $SELECTED_UTXO_LOVELACE"
    # echo "🪙 Token: $SELECTED_UTXO_TOKENS $SELECTED_UTXO_ASSET"
}


function showTokenBalance(){
	rm -f -r tmp
	mkdir tmp
	touch tmp/utxos.txt
	UTXOS_FILE="./tmp/utxos.txt"

	if [ -z "$1" ]
	then
		read -p "Enter Wallet Directory: " WALLET_DIRECTORY
	else
		WALLET_DIRECTORY=$1
	fi

	WALLET_ADDRESS=$(cat $WALLET_DIRECTORY/payment.addr)
	WALLET_KEY="$WALLET_DIRECTORY/payment.skey"

	# Get balance (UTxOs)
	./balance.sh $WALLET_ADDRESS > $UTXOS_FILE

	# Ask user for token name
	read -p "Enter the token name (exact match required): " TOKEN_NAME
	TOKEN_HEXSTRING=$(xxd -pu <<<"$TOKEN_NAME")
	TOKEN_HEX=${TOKEN_HEXSTRING::-2}

	# Filter UTxOs that contain the specified token
	echo "Searching for UTxOs containing token: $TOKEN_NAME ($TOKEN_HEX)"
	grep "$TOKEN_HEX" $UTXOS_FILE > tmp.txt

	if [ ! -s tmp.txt ]; then
		echo "❌ No UTxOs found containing token: $TOKEN_NAME"
		return 1
	fi

	# Calculate total token balance
	TOTAL_TOKENS=0
	while read -r line
	do
	  TOKEN_AMOUNT=$(echo "$line" | awk '{ print $6 }')
	  TOTAL_TOKENS=$((TOTAL_TOKENS + TOKEN_AMOUNT))
	done < tmp.txt

	echo "✅ Total balance of $TOKEN_NAME: $TOTAL_TOKENS"
}

function selectAllUTXO(){
	rm -f -r tmp
	mkdir tmp
	touch tmp/utxos.txt
	UTXOS_FILE="./tmp/utxos.txt"

	if [ -z "$1" ]
	then
		read -p "Enter Wallet Directory: " WALLET_DIRECTORY
	else
		WALLET_DIRECTORY=$1
	fi

	WALLET_ADDRESS=$(cat $WALLET_DIRECTORY/payment.addr)
	WALLET_KEY="$WALLET_DIRECTORY/payment.skey"

	# Get balance (UTxOs)
	./balance.sh $WALLET_ADDRESS > $UTXOS_FILE

	# Check if UTXO file is empty
	if [ ! -s "$UTXOS_FILE" ]; then
	    echo "⚠️ No UTXOs found! Check if balance.sh is working properly."
	    exit 1
	fi

	# Extract all UTXOs
	UTXO_LIST=""
	TOTAL_LOVELACE=0
	UTXO_COUNT=0  # Initialize counter
	TOTAL_TOKENS=0

	# Read each line after skipping first 2 lines
	while IFS= read -r line; do
	  TX_ID=$(echo "$line" | awk '{ print $1 }')
	  TX_INDEX=$(echo "$line" | awk '{ print $2 }')
	  AMOUNT=$(echo "$line" | awk '{ print $3 }')
	  UTXO_LIST="$UTXO_LIST --tx-in $TX_ID#$TX_INDEX"
	  TOTAL_LOVELACE=$((TOTAL_LOVELACE + AMOUNT))
	  ((UTXO_COUNT++))  # Increment counter
	done < <(tail -n +3 "$UTXOS_FILE")

	# echo "💰 Total Lovelace to be sent: $TOTAL_LOVELACE"
	# echo "✅ UTXO_LIST: $UTXO_LIST"
	# echo "🔢 Total UTXOs: $UTXO_COUNT"

}