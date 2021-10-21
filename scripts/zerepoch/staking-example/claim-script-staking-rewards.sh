#!/usr/bin/env bash

set -e
# Unoffiical bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail


export BASE="${BASE:-.}"
export WORK="${WORK:-example/work}"
export BCC_NODE_SOCKET_PATH="${BCC_NODE_SOCKET_PATH:-example/node-bft1/node.sock}"
export TESTNET_MAGIC="${TESTNET_MAGIC:-42}"
export UTXO_VKEY1="${UTXO_VKEY1:-example/sophie/utxo-keys/utxo1.vkey}"
export UTXO_SKEY1="${UTXO_SKEY1:-example/sophie/utxo-keys/utxo1.skey}"
export UTXO_VKEY2="${UTXO_VKEY1:-example/sophie/utxo-keys/utxo2.vkey}"
export UTXO_SKEY2="${UTXO_SKEY1:-example/sophie/utxo-keys/utxo2.skey}"
export UTXO_STAKING_VKEY1="${UTXO_STAKING_VKEY1:=example/sophie/utxo-keys/utxo-stake.vkey}"
export UTXO_STAKING_SKEY1="${UTXO_STAKING_SKEY1:=example/sophie/utxo-keys/utxo-stake.skey}"
export UTXO_STAKING_VKEY2="${UTXO_STAKING_VKEY2:=example/sophie/utxo-keys/utxo2-stake.vkey}"
export UTXO_STAKING_SKEY2="${UTXO_STAKING_SKEY2:=example/sophie/utxo-keys/utxo2-stake.skey}"

utxoaddr=$(bcc-cli address build --testnet-magic "$TESTNET_MAGIC" --payment-verification-key-file "$UTXO_VKEY1")


bcc-cli query utxo \
  --address "$utxoaddr" \
  --bcc-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file "$WORK/utxo-1.json"

echo "UTxO"
cat "$WORK/utxo-1.json"
echo ""

txin=$(jq -r 'keys[0]' $WORK/utxo-1.json)
txinentropic=$(jq -r ".[\"$txin\"].value.entropic" $WORK/utxo-1.json)
txincollateral=$(jq -r 'keys[1]' $WORK/utxo-1.json)
scriptpaymentaddrwithstakecred=$(bcc-cli address build --payment-verification-key-file $UTXO_VKEY1  --stake-script-file "scripts/zerepoch/scripts/guess-42-stake.zerepoch" --testnet-magic 42)
stakingscriptaddr=$(bcc-cli stake-address build --stake-script-file scripts/zerepoch/scripts/guess-42-stake.zerepoch --testnet-magic 42)

# STEP 1 - Get reward account balance

bcc-cli query stake-address-info \
  --address "$stakingscriptaddr" \
  --testnet-magic 42 \
  --out-file "$WORK/scriptdelegationstatusrewards.json"

rewardamt=$(jq -r '.[0].rewardAccountBalance' $WORK/scriptdelegationstatusrewards.json)

totalspendable=$(expr $rewardamt + $txinentropic - 289563)
echo "Entropic at utxo: $txinentropic"
echo "Rewards: $rewardamt"
echo "Combined: $totalspendable"

bcc-cli transaction build \
  --aurum-era \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$txin" \
  --tx-in-collateral "$txincollateral" \
  --tx-out "$scriptpaymentaddrwithstakecred+$totalspendable" \
  --withdrawal "$stakingscriptaddr+$rewardamt" \
  --withdrawal-script-file "scripts/zerepoch/scripts/guess-42-stake.zerepoch" \
  --withdrawal-redeemer-file "scripts/zerepoch/data/42.redeemer" \
  --protocol-params-file "$WORK/pparams.json" \
  --out-file "$WORK/script-withdrawal.txbody"

bcc-cli transaction sign \
  --tx-body-file "$WORK/script-withdrawal.txbody" \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "$UTXO_SKEY1" \
  --out-file "$WORK/script-withdrawal.tx"

echo "Submitting withdrawal..."

bcc-cli transaction submit \
  --tx-file "$WORK/script-withdrawal.tx" \
  --testnet-magic "$TESTNET_MAGIC"

echo "Waiting 5 seconds...."
sleep 5

bcc-cli query stake-address-info \
  --address "$stakingscriptaddr" \
  --testnet-magic 42 \
  --out-file "$WORK/scriptrewardscheck.json"

scriptrewardscheck=$(jq -r '.[0]' $WORK/scriptrewardscheck.json)
echo "Checking if script rewards withdrawal was successful..."
echo "$scriptrewardscheck"
