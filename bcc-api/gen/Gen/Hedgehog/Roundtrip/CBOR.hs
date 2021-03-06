{-# LANGUAGE FlexibleContexts #-}

module Gen.Hedgehog.Roundtrip.CBOR
  ( roundtrip_CBOR
  ) where

import           Bcc.Api
import           Bcc.Prelude
import           Hedgehog (Gen, Property)

import qualified Hedgehog as H

{- HLINT ignore "Use camelCase" -}

roundtrip_CBOR
  :: (SerialiseAsCBOR a, Eq a, Show a)
  => AsType a -> Gen a -> Property
roundtrip_CBOR typeProxy gen =
  H.property $ do
    val <- H.forAll gen
    H.tripping val serialiseToCBOR (deserialiseFromCBOR typeProxy)
