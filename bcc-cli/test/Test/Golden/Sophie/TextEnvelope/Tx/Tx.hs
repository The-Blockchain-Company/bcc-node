{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Sophie.TextEnvelope.Tx.Tx
  ( golden_sophieTx
  ) where

import           Bcc.Api (AsType (..), HasTextEnvelope (..))
import           Bcc.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H

{- HLINT ignore "Use camelCase" -}

-- | 1. Generate a key pair
--   2. Create tx body
--   3. Sign tx body
--   4. Check the TextEnvelope serialization format has not changed.
golden_sophieTx :: Property
golden_sophieTx = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  let referenceTx = "test/data/golden/sophie/tx/tx"

  -- Key filepaths
  paymentVerKey <- noteTempFile tempDir "payment-verification-key-file"
  paymentSignKey <- noteTempFile tempDir "payment-signing-key-file"
  transactionFile <- noteTempFile tempDir "tx-file"
  transactionBodyFile <- noteTempFile tempDir "tx-body-file"

  -- Generate payment signing key to sign transaction
  void $ execBccCLI
    [ "address","key-gen"
    , "--verification-key-file", paymentVerKey
    , "--signing-key-file", paymentSignKey
    ]

  -- Create transaction body
  void $ execBccCLI
    [ "transaction", "build-raw"
    , "--sophie-era"
    , "--tx-in", "91999ea21177b33ebe6b8690724a0c026d410a11ad7521caa350abdafa5394c3#0"
    , "--tx-out", "addr1v9wmu83pzajplrtpsq6tsqdgwr98x888trpmah2u0ezznsge7del3+100000000"
    , "--fee", "1000000"
    , "--invalid-hereafter", "500000"
    , "--out-file", transactionBodyFile
    ]

  -- Sign transaction
  void $ execBccCLI
    [ "transaction", "sign"
    , "--tx-body-file", transactionBodyFile
    , "--signing-key-file", paymentSignKey
    , "--mainnet"
    , "--out-file", transactionFile
    ]

  let txType = textEnvelopeType AsSophieTx

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTextEnvelopeFormat txType referenceTx transactionFile
