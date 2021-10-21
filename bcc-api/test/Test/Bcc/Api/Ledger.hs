{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Bcc.Api.Ledger
  ( tests
  ) where

import           Bcc.Prelude

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Aeson as H
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (testGroupGenerator)

import           Bcc.Ledger.Address (deserialiseAddr, serialiseAddr)
import           Shardagnostic.Consensus.Sophie.Eras (StandardCrypto)
import           Test.Bcc.Api.Genesis
import           Test.Bcc.Ledger.Sophie.Serialisation.Generators.Genesis (genAddress)

prop_golden_SophieGenesis :: Property
prop_golden_SophieGenesis = H.goldenTestJsonValuePretty exampleSophieGenesis "test/Golden/SophieGenesis"

-- Keep this here to make sure serialiseAddr/deserialiseAddr are working.
-- They are defined in the Sophie executable spec and have been wrong at
-- least once.
prop_roundtrip_Address_CBOR :: Property
prop_roundtrip_Address_CBOR = H.property $ do
  -- If this fails, FundPair and SophieGenesis can also fail.
  addr <- H.forAll (genAddress @StandardCrypto)
  H.tripping addr serialiseAddr deserialiseAddr

-- -----------------------------------------------------------------------------

tests :: TestTree
tests = $testGroupGenerator
