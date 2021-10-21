{-# LANGUAGE TemplateHaskell #-}
module Test.Bcc.Api.Typed.Bech32
  ( tests
  ) where

import           Hedgehog (Property)
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (testGroupGenerator)

import           Bcc.Api
import           Gen.Bcc.Api.Typed
import           Gen.Hedgehog.Roundtrip.Bech32 (roundtrip_Bech32)

prop_roundtrip_Address_Sophie :: Property
prop_roundtrip_Address_Sophie = roundtrip_Bech32 AsSophieAddress genAddressSophie

prop_roundtrip_StakeAddress :: Property
prop_roundtrip_StakeAddress = roundtrip_Bech32 AsStakeAddress genStakeAddress

tests :: TestTree
tests = $testGroupGenerator
