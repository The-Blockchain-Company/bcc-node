{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Bcc.Api.Typed.Envelope
  ( tests
  ) where

import           Bcc.Api
import           Bcc.Prelude
import           Gen.Bcc.Api.Typed
import           Gen.Tasty.Hedgehog.Group (fromGroup)
import           Hedgehog (Property, discover)
import           Test.Bcc.Api.Typed.Orphans ()
import           Test.Tasty (TestTree)

import qualified Hedgehog as H

{- HLINT ignore "Use camelCase" -}

prop_roundtrip_ColeVerificationKey_envelope :: Property
prop_roundtrip_ColeVerificationKey_envelope =
  roundtrip_VerificationKey_envelope AsColeKey

prop_roundtrip_ColeSigningKey_envelope :: Property
prop_roundtrip_ColeSigningKey_envelope =
  roundtrip_SigningKey_envelope AsColeKey

prop_roundtrip_PaymentVerificationKey_envelope :: Property
prop_roundtrip_PaymentVerificationKey_envelope =
  roundtrip_VerificationKey_envelope AsPaymentKey

prop_roundtrip_PaymentSigningKey_envelope :: Property
prop_roundtrip_PaymentSigningKey_envelope =
  roundtrip_SigningKey_envelope AsPaymentKey


prop_roundtrip_StakeVerificationKey_envelope :: Property
prop_roundtrip_StakeVerificationKey_envelope =
  roundtrip_VerificationKey_envelope AsStakeKey

prop_roundtrip_StakeSigningKey_envelope :: Property
prop_roundtrip_StakeSigningKey_envelope =
  roundtrip_SigningKey_envelope AsStakeKey


prop_roundtrip_StakePoolVerificationKey_envelope :: Property
prop_roundtrip_StakePoolVerificationKey_envelope =
  roundtrip_VerificationKey_envelope AsStakePoolKey

prop_roundtrip_StakePoolSigningKey_envelope :: Property
prop_roundtrip_StakePoolSigningKey_envelope =
  roundtrip_SigningKey_envelope AsStakePoolKey


prop_roundtrip_GenesisVerificationKey_envelope :: Property
prop_roundtrip_GenesisVerificationKey_envelope =
  roundtrip_VerificationKey_envelope AsGenesisKey

prop_roundtrip_GenesisSigningKey_envelope :: Property
prop_roundtrip_GenesisSigningKey_envelope =
  roundtrip_SigningKey_envelope AsGenesisKey


prop_roundtrip_GenesisDelegateVerificationKey_envelope :: Property
prop_roundtrip_GenesisDelegateVerificationKey_envelope =
  roundtrip_VerificationKey_envelope AsGenesisDelegateKey

prop_roundtrip_GenesisDelegateSigningKey_envelope :: Property
prop_roundtrip_GenesisDelegateSigningKey_envelope =
  roundtrip_SigningKey_envelope AsGenesisDelegateKey


prop_roundtrip_KesVerificationKey_envelope :: Property
prop_roundtrip_KesVerificationKey_envelope =
  roundtrip_VerificationKey_envelope AsKesKey

prop_roundtrip_KesSigningKey_envelope :: Property
prop_roundtrip_KesSigningKey_envelope =
  roundtrip_SigningKey_envelope AsKesKey


prop_roundtrip_VrfVerificationKey_envelope :: Property
prop_roundtrip_VrfVerificationKey_envelope =
  roundtrip_VerificationKey_envelope AsVrfKey

prop_roundtrip_VrfSigningKey_envelope :: Property
prop_roundtrip_VrfSigningKey_envelope =
  roundtrip_SigningKey_envelope AsVrfKey

-- -----------------------------------------------------------------------------

roundtrip_VerificationKey_envelope :: Key keyrole
                                   => AsType keyrole -> Property
roundtrip_VerificationKey_envelope roletoken =
  H.property $ do
    vkey <- H.forAll (genVerificationKey roletoken)
    H.tripping vkey (serialiseToTextEnvelope Nothing)
                    (deserialiseFromTextEnvelope (AsVerificationKey roletoken))

roundtrip_SigningKey_envelope :: (Key keyrole,
                                  Eq (SigningKey keyrole),
                                  Show (SigningKey keyrole))
                              => AsType keyrole -> Property
roundtrip_SigningKey_envelope roletoken =
  H.property $ do
    vkey <- H.forAll (genSigningKey roletoken)
    H.tripping vkey (serialiseToTextEnvelope Nothing)
                    (deserialiseFromTextEnvelope (AsSigningKey roletoken))

-- -----------------------------------------------------------------------------

tests :: TestTree
tests = fromGroup $$discover
