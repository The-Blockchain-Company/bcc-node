#!/usr/bin/env bash

# This script iterates through the txouts of an address and splits each of them into two smaller txouts
#
# The script takes one argument, which is the maximum number of txouts to split.

set -eo pipefail

export WORK="${WORK:-example/work}"
export BASE="${BASE:-.}"
export BCC_CLI="${BCC_CLI:-bcc-cli}"
export BCC_NODE_SOCKET_PATH="${BCC_NODE_SOCKET_PATH:-example/node-bft1/node.sock}"
export TESTNET_MAGIC="${TESTNET_MAGIC:-42}"
export UTXO_VKEY="${UTXO_VKEY:-example/sophie/utxo-keys/utxo1.vkey}"
export UTXO_SKEY="${UTXO_SKEY:-example/sophie/utxo-keys/utxo1.skey}"
export RESULT_FILE_TARGET="${RESULT_FILE:-$WORK/target.out}"
export RESULT_FILE_CHANGE="${RESULT_FILE:-$WORK/change.out}"

echo "Socket path: $BCC_NODE_SOCKET_PATH"
echo "Socket path: $(pwd)"

count="${1:-1000000}"

mkdir -p "$WORK"

utxoaddr=$($BCC_CLI address build --testnet-magic "$TESTNET_MAGIC" --payment-verification-key-file "$UTXO_VKEY")

echo "Built new address: utxoaddr=$utxoaddr"

$BCC_CLI query utxo --address "$utxoaddr" --bcc-mode --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/utxo-1.json

echo "Queried UTxO for address: utxoaddr=$utxoaddr"

txins="$(jq -r ". | to_entries | sort_by(.value.value.entropic) | reverse | map(.key)[0:$count][]" $WORK/utxo-1.json)"

for txin in $txins; do
  entropicattxin="$(jq -r ".[\"$txin\"].value.entropic" $WORK/utxo-1.json)"
  entropicattxindiv2="$(expr $entropicattxin / 2)"

  targetaddr="$utxoaddr"

  $BCC_CLI transaction build \
    --aurum-era \
    --bcc-mode \
    --testnet-magic "$TESTNET_MAGIC" \
    --change-address "$utxoaddr" \
    --tx-in $txin \
    --tx-out "$targetaddr+$entropicattxindiv2" \
    --out-file "$WORK/build.body"

  $BCC_CLI transaction sign \
    --tx-body-file "$WORK/build.body" \
    --testnet-magic "$TESTNET_MAGIC" \
    --signing-key-file $UTXO_SKEY \
    --out-file "$WORK/build.tx.signed"

  if [ "$SUBMIT_API_PORT" != "" ]; then
    xxd -r -p <<< $(jq .cborHex $WORK/build.tx.signed) > $WORK/build.tx.signed.cbor

    curl \
      -s \
      --header "Content-Type: application/cbor" \
      -X POST "http://localhost:$SUBMIT_API_PORT/api/submit/tx" \
      --data-binary "@$WORK/build.tx.signed.cbor"
  else
    $BCC_CLI transaction submit --tx-file "$WORK/build.tx.signed" --testnet-magic "$TESTNET_MAGIC"
  fi
done

echo "Submitted $(echo "$txins" | wc -w) transactions"
