{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Bcc.Api.Typed.Address
  ( tests
  ) where

import           Bcc.Prelude

import           Hedgehog (Property)
import qualified Hedgehog as H
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (testGroupGenerator)

import           Bcc.Api
import           Gen.Bcc.Api.Typed
import           Test.Bcc.Api.Typed.Orphans ()

{- HLINT ignore "Use camelCase" -}

-- Address CBOR round trips

prop_roundtrip_sophie_address :: Property
prop_roundtrip_sophie_address =
  roundtrip_serialise_address AsSophieAddress genAddressSophie


prop_roundtrip_cole_address :: Property
prop_roundtrip_cole_address =
  roundtrip_serialise_address AsColeAddress genAddressCole


-- -----------------------------------------------------------------------------

roundtrip_serialise_address
  :: ( SerialiseAddress a
     , Eq a
     , Show a) => AsType a -> H.Gen a -> Property
roundtrip_serialise_address asType g =
  H.property $ do
    v <- H.forAll g
    H.tripping v serialiseAddress (deserialiseAddress asType)


-- -----------------------------------------------------------------------------

tests :: TestTree
tests = $testGroupGenerator
