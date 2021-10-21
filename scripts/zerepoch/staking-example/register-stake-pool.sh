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

# TODO: Left off here. You should try a staking key address to see if you are the problem!
mkdir -p "$WORK"

utxoaddr=$(bcc-cli address build --testnet-magic "$TESTNET_MAGIC" --payment-verification-key-file "$UTXO_VKEY1")
nodepool1dir=example/node-pool1
utxoaddrwithstaking=$(bcc-cli address build --payment-verification-key-file "$UTXO_VKEY2" --stake-verification-key-file "$UTXO_STAKING_VKEY2" --testnet-magic 42)
keystakeaddress=$(bcc-cli stake-address build --stake-verification-key-file "$UTXO_STAKING_VKEY2" --testnet-magic 42)


bcc-cli query utxo \
  --address "$utxoaddr" \
  --bcc-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file "$WORK/utxo-1.json"

echo "UTxO"
cat "$WORK/utxo-1.json"
echo ""

txin=$(jq -r 'keys[]' $WORK/utxo-1.json)
entropicattxin=$(jq -r ".[\"$txin\"].value.entropic" $WORK/utxo-1.json)
entropicattxindiv3=$(expr $entropicattxin / 3)
scriptpaymentaddrwithstakecred=$(bcc-cli address build --payment-verification-key-file $UTXO_VKEY1  --stake-script-file "scripts/zerepoch/scripts/guess-42-stake.zerepoch" --testnet-magic 42)
#TODO: Look at stake-distbution cmd
poolownerstakekey="example/addresses/pool-owner1-stake.vkey"
poolowneraddresswstakecred=$(bcc-cli address build --payment-verification-key-file  example/addresses/pool-owner1.vkey --stake-verification-key-file example/addresses/pool-owner1-stake.vkey --testnet-magic 42)
poolcoldkey="example/node-pool1/sophie/operator.vkey"

#Register stake pool

# We need to submit the stake pool registration certificate and
# also submit the delegation certificate of the pledger

# STEP 1
# Create registration certificate of pledger AND FUND THE POOL OWNER'S ADDRESS

bcc-cli stake-address registration-certificate \
  --stake-verification-key-file "$poolownerstakekey" \
  --out-file "$WORK/pledger.regcert"

bcc-cli transaction build \
  --aurum-era \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$txin" \
  --tx-out "$scriptpaymentaddrwithstakecred+5000000" \
  --tx-out "$poolowneraddresswstakecred+5000000" \
  --tx-out "$utxoaddrwithstaking+5000000" \
  --witness-override 3 \
  --certificate-file "$WORK/pledger.regcert" \
  --out-file "$WORK/pledge-registration-cert.txbody"

bcc-cli transaction sign \
  --tx-body-file "$WORK/pledge-registration-cert.txbody" \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "$UTXO_SKEY1" \
  --out-file "$WORK/pledge-registration-cert.tx"

echo "Submitting pool owner/pledge stake registration cert and funding stake pool owner address..."

bcc-cli transaction submit \
  --tx-file "$WORK/pledge-registration-cert.tx" \
  --testnet-magic "$TESTNET_MAGIC"

poolownerstakeaddr=$(bcc-cli stake-address build --stake-verification-key-file $poolownerstakekey --testnet-magic 42)

echo ""
echo "Pool owner/pledger stake address"
echo "$poolownerstakeaddr"
echo "Waiting 10 seconds..."

sleep 10

# Check the stake address was registered
bcc-cli query stake-address-info \
  --address "$poolownerstakeaddr" \
  --testnet-magic 42 \
  --out-file "$WORK/pledgeownerregistration.json"
registered=$(jq -r '.[0]' $WORK/pledgeownerregistration.json)

echo ""
echo "Registered pool owner/pledger address. If null it was not successfully registered"
echo "$registered"
sleep 2

#Register key staking address
echo ""
echo "Register key staking address"
echo "$poolownerstakeaddr"
echo ""

bcc-cli query utxo \
  --address "$utxoaddrwithstaking" \
  --bcc-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file "$WORK/staking-key-utxo-1.json"

echo "Staking key UTxO"
cat "$WORK/staking-key-utxo-1.json"
echo ""

keytxin=$(jq -r 'keys[]' $WORK/staking-key-utxo-1.json)

bcc-cli stake-address registration-certificate \
  --stake-verification-key-file "$UTXO_STAKING_VKEY2" \
  --out-file "$WORK/stakekey.regcert"

bcc-cli transaction build \
  --aurum-era \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddrwithstaking" \
  --tx-in "$keytxin" \
  --tx-out "$utxoaddrwithstaking+1000" \
  --witness-override 3 \
  --certificate-file "$WORK/stakekey.regcert" \
  --out-file "$WORK/key-registration-cert.txbody"

bcc-cli transaction sign \
  --tx-body-file "$WORK/key-registration-cert.txbody" \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "$UTXO_SKEY2" \
  --signing-key-file "$UTXO_STAKING_SKEY2" \
  --out-file "$WORK/key-registration-cert.tx"

echo "Submitting key stake registration cert..."

bcc-cli transaction submit \
  --tx-file "$WORK/key-registration-cert.tx" \
  --testnet-magic "$TESTNET_MAGIC"
echo "Wait 10 seconds..."
sleep 10

echo "Check to see if it was registered..."
bcc-cli query stake-address-info \
  --address "$keystakeaddress" \
  --testnet-magic 42 \
  --out-file "$WORK/keyregistration.json"

registeredkey=$(jq -r '.[0]' $WORK/keyregistration.json)
echo ""
echo "Registered key staking address. If null it was not successfully registered"
echo "$registeredkey"


# Update UTxO

bcc-cli query utxo \
  --address "$utxoaddr" \
  --bcc-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file "$WORK/utxo-1.json"

cat "$WORK/utxo-1.json"

txinupdated=$(jq -r 'keys[0]' $WORK/utxo-1.json)


# STEP 2
# Create delegation certificate of pledger

bcc-cli stake-address delegation-certificate \
  --stake-verification-key-file "$poolownerstakekey" \
  --cold-verification-key-file "$poolcoldkey" \
  --out-file "$WORK/pledger.delegcert"

# TODO: We use witness override because the build command does not take certificates
# into account and underestimates the tx fee.

# STEP 3
# REGISTER STAKE POOL AND DELEGATE THE PLEDGER TO THE STAKE POOL IN ONE TX

bcc-cli transaction build \
  --aurum-era \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$txinupdated" \
  --tx-out "$scriptpaymentaddrwithstakecred+500" \
  --witness-override 3 \
  --certificate-file "example/node-pool1/registration.cert" \
  --certificate-file "$WORK/pledger.delegcert" \
  --out-file "$WORK/register-stake-pool.txbody"

# UTxO Payment key
# Staking key
# Cold key
bcc-cli transaction sign \
  --tx-body-file "$WORK/register-stake-pool.txbody" \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "$UTXO_SKEY1" \
  --signing-key-file "$nodepool1dir/sophie/operator.skey" \
  --signing-key-file "$nodepool1dir/owner.skey" \
  --out-file "$WORK/register-stake-pool.tx"

echo "Registering stake pool and delegating to said stake pool"

bcc-cli transaction submit \
  --tx-file "$WORK/register-stake-pool.tx" \
  --testnet-magic "$TESTNET_MAGIC"

echo "Wait 5 seconds for UTxO to update..."
sleep 5
currentstakepools=$(bcc-cli query stake-pools --testnet-magic 42)

# Check the stake address was delegated
bcc-cli query stake-address-info \
  --address "$poolownerstakeaddr" \
  --testnet-magic 42 \
  --out-file "$WORK/pledgeownerregistration.json"
delegated=$(jq -r '.[0]' $WORK/pledgeownerregistration.json)

echo ""
echo "Currently registered stake pools"
echo "$currentstakepools"
echo ""
echo "We check if the pool owner/pledger has successfully delegated"
echo "$delegated"


echo ""
echo "Delegate staking key to stake pool"
echo ""

bcc-cli stake-address delegation-certificate \
  --stake-verification-key-file "$UTXO_STAKING_VKEY2" \
  --cold-verification-key-file "$poolcoldkey" \
  --out-file "$WORK/stakekey.delegcert"

bcc-cli query utxo \
  --address "$utxoaddrwithstaking" \
  --bcc-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file "$WORK/staking-key-utxo-2.json"

echo "Staking key UTxO"
cat "$WORK/staking-key-utxo-2.json"
echo ""

keytxin2=$(jq -r 'keys[0]' $WORK/staking-key-utxo-2.json)

bcc-cli transaction build \
  --aurum-era \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddrwithstaking" \
  --tx-in "$keytxin2" \
  --tx-out "$utxoaddrwithstaking+1000" \
  --witness-override 3 \
  --certificate-file "$WORK/stakekey.delegcert" \
  --out-file "$WORK/key-deleg-cert.txbody"

bcc-cli transaction sign \
  --tx-body-file "$WORK/key-deleg-cert.txbody" \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "$UTXO_SKEY2" \
  --signing-key-file "$UTXO_STAKING_SKEY2" \
  --out-file "$WORK/key-deleg-cert.tx"

echo "Submitting key stake delegation cert"

bcc-cli transaction submit \
  --tx-file "$WORK/key-deleg-cert.tx" \
  --testnet-magic "$TESTNET_MAGIC"

echo "Wait 10 seconds..."
sleep 10

echo "Check to see if it was delegated..."
bcc-cli query stake-address-info \
  --address "$keystakeaddress" \
  --testnet-magic 42 \
  --out-file "$WORK/keydelegation.json"

delegatedkey=$(jq -r '.[0]' $WORK/keydelegation.json)
echo ""
echo "Delegating key staking address. If null it was not successfully registered"
echo "$delegatedkey"



# UP TO HERE IN THEORY WE ARE FINE. THE POOL GETS REGISTERED AND THE PLEDGE IS DELEGATED TO THE POOL
# Delegate Zerepoch staking script address to stake pool

# Update UTxO again

bcc-cli query utxo \
  --address "$utxoaddr" \
  --bcc-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file "$WORK/utxo-2.json"

cat "$WORK/utxo-2.json"

txinupdated2=$(jq -r 'keys[0]' $WORK/utxo-2.json)
echo ""
echo "Selected txin: $txinupdated2"
# Step 1: Create registration certificate for the staking script

# We also create collateral.

txin=$(jq -r 'keys[]' $WORK/utxo-2.json)
entropicattxin=$(jq -r ".[\"$txin\"].value.entropic" $WORK/utxo-2.json)
entropicattxindiv3=$(expr $entropicattxin / 3)

bcc-cli stake-address registration-certificate \
  --stake-script-file "scripts/zerepoch/scripts/guess-42-stake.zerepoch" \
  --out-file "$WORK/script.regcert"

bcc-cli transaction build \
  --aurum-era \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$txinupdated2" \
  --tx-out "$scriptpaymentaddrwithstakecred+500" \
  --tx-out "$utxoaddr+$entropicattxindiv3" \
  --witness-override 3 \
  --certificate-file "$WORK/script.regcert" \
  --out-file "$WORK/script-registration-cert.txbody"

bcc-cli transaction sign \
  --tx-body-file "$WORK/script-registration-cert.txbody" \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "$UTXO_SKEY1" \
  --out-file "$WORK/script-registration-cert.tx"

bcc-cli transaction submit \
  --tx-file "$WORK/script-registration-cert.tx" \
  --testnet-magic "$TESTNET_MAGIC"

stakingscriptaddr=$(bcc-cli stake-address build --stake-script-file scripts/zerepoch/scripts/guess-42-stake.zerepoch --testnet-magic 42)

echo ""
echo "Staking script address"
echo "$stakingscriptaddr"
echo "Waiting 10 seconds..."
sleep 10
echo "Check to see if the SCRIPT staking address was successfully REGISTERED"

bcc-cli query stake-address-info \
  --address "$stakingscriptaddr" \
  --testnet-magic 42 \
  --out-file "$WORK/scriptregistration.json"

registeredscr=$(jq -r '.[0]' $WORK/scriptregistration.json)
echo "$registeredscr"

# We have successfully registered our script staking address.

# We need to delegate the script staking address

bcc-cli stake-address delegation-certificate \
  --stake-script-file "scripts/zerepoch/scripts/guess-42-stake.zerepoch" \
  --cold-verification-key-file "$poolcoldkey" \
  --out-file "$WORK/script.delegcert"

bcc-cli query protocol-parameters --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/pparams.json

# We also need collateral

bcc-cli query utxo \
  --address "$utxoaddr" \
  --bcc-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file "$WORK/utxo-2.json"

cat "$WORK/utxo-2.json"

txinupdated3=$(jq -r 'keys[0]' $WORK/utxo-2.json)
txincollateral=$(jq -r 'keys[1]' $WORK/utxo-2.json)
echo ""
echo "Selected txin: $txinupdated2"

bcc-cli transaction build \
  --aurum-era \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$txinupdated3" \
  --tx-in-collateral "$txincollateral" \
  --tx-out "$scriptpaymentaddrwithstakecred+500" \
  --witness-override 3 \
  --certificate-file "$WORK/script.delegcert" \
  --certificate-script-file "scripts/zerepoch/scripts/guess-42-stake.zerepoch" \
  --certificate-redeemer-file "scripts/zerepoch/data/42.redeemer" \
  --protocol-params-file "$WORK/pparams.json" \
  --out-file "$WORK/script-delegation-cert.txbody"

bcc-cli transaction sign \
  --tx-body-file "$WORK/script-delegation-cert.txbody" \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "$UTXO_SKEY1" \
  --out-file "$WORK/script-delegation-cert.tx"

echo "Submitting staking script delegation certificate..."

bcc-cli transaction submit \
  --tx-file "$WORK/script-delegation-cert.tx" \
  --testnet-magic "$TESTNET_MAGIC"

echo "Waiting 10 seconds..."
sleep 10
echo "Check to see if staking script was successfully delegated..."


bcc-cli query stake-address-info \
  --address "$stakingscriptaddr" \
  --testnet-magic 42 \
  --out-file "$WORK/scriptdelegation.json"

delegatedscript=$(jq -r '.[0]' $WORK/scriptdelegation.json)
echo "$delegatedscript"
echo ""
echo "Stake payment address"
echo "$scriptpaymentaddrwithstakecred"
# We have two scenarios to test, deregistration and rewards withdrawal
# SCENARIO 1: WITHDRAWAL


# SCENARIO 2: DEREGISTRATION -- THIS WORKS

# Update UTxO again
#echo ""
#echo "Script staking address deregistration"
#echo ""
#bcc-cli query utxo \
#  --address "$utxoaddr" \
#  --bcc-mode \
#  --testnet-magic "$TESTNET_MAGIC" \
#  --out-file "$WORK/utxo-2.json"
#
#cat "$WORK/utxo-2.json"
#
#txinupdated3=$(jq -r 'keys[0]' $WORK/utxo-2.json)
#txincollateral=$(jq -r 'keys[1]' $WORK/utxo-2.json)
#echo ""
#echo "Selected txin: $txinupdated2"
#
## Create deregistration certificate
#bcc-cli stake-address deregistration-certificate \
#  --stake-script-file "scripts/zerepoch/scripts/guess-42-stake.zerepoch" \
#  --out-file "$WORK/script.deregcert"
#
#
## TODO: Then figure out why rewards aren't being dispersed.
#
## Get PParams
#
#bcc-cli query protocol-parameters --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/pparams.json
#
#bcc-cli transaction build \
#  --aurum-era \
#  --testnet-magic "$TESTNET_MAGIC" \
#  --change-address "$utxoaddr" \
#  --tx-in "$txinupdated3" \
#  --tx-in-collateral "$txincollateral" \
#  --tx-out "$scriptpaymentaddrwithstakecred+500" \
#  --witness-override 3 \
#  --certificate-file "$WORK/script.deregcert" \
#  --certificate-script-file "scripts/zerepoch/scripts/guess-42-stake.zerepoch" \
#  --certificate-redeemer-file "scripts/zerepoch/data/42.redeemer" \
#  --protocol-params-file "$WORK/pparams.json" \
#  --out-file "$WORK/script-deregistration-cert.txbody"
#
#bcc-cli transaction sign \
#  --tx-body-file "$WORK/script-deregistration-cert.txbody" \
#  --testnet-magic "$TESTNET_MAGIC" \
#  --signing-key-file "$UTXO_SKEY1" \
#  --out-file "$WORK/script-deregistration-cert.tx"
#
#bcc-cli transaction submit \
#  --tx-file "$WORK/script-deregistration-cert.tx" \
#  --testnet-magic "$TESTNET_MAGIC"
#
#echo "Staking script adress"
#echo "$stakingscriptaddr"
#echo "Waiting 5 seconds..."
#sleep 5
#echo "Check to see if the script staking address was successfully deregistered"
#
#bcc-cli query stake-address-info \
#  --address "$stakingscriptaddr" \
#  --testnet-magic 42 \
#  --out-file "$WORK/scriptderegistration.json"
#
#deregistered=$(jq -r '.[0]' $WORK/scriptderegistration.json)
#echo "$deregistered"


