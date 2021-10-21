#!/usr/bin/env bash

set -e
# Unoffiical bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

export BCC_NODE_SOCKET_PATH="${BCC_NODE_SOCKET_PATH:-example/node-bft1/node.sock}"


# query the UTxO
bcc-cli query utxo \
            --address "$(cat addresses/address.addr)" \
            --bcc-mode \
            --testnet-magic ${TESTNET_MAGIC} \
            --out-file queries/utxo.json

# bcc-cli transaction build-raw
TXIN=$(jq -r 'keys[0]' queries/utxo.json)
ENTROPIC=$(jq -r ".[\"$TXIN\"].value.entropic" queries/utxo.json)

mkdir -p txs

bcc-cli transaction build \
            --aurum-era \
            --bcc-mode \
            --testnet-magic ${TESTNET_MAGIC} \
            --tx-in ${TXIN} \
            --tx-out "$(cat addresses/address.addr)+1000000" \
            --change-address $(cat addresses/address-script.addr) \
            --out-file txs/create-collateral.txraw

bcc-cli transaction sign \
            --tx-body-file txs/create-collateral.txraw \
            --signing-key-file addresses/payment-addr.skey \
            --testnet-magic ${TESTNET_MAGIC} \
            --out-file txs/create-collateral

bcc-cli transaction submit \
            --bcc-mode \
            --testnet-magic ${TESTNET_MAGIC} \
            --tx-file txs/create-collateral
