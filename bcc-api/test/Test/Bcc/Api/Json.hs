{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Bcc.Api.Json
  ( tests
  ) where

import           Bcc.Prelude

import           Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), eitherDecode, encode)
import           Data.Aeson.Types (Parser, parseEither)
import           Hedgehog (Property, forAll, tripping)
import qualified Hedgehog as H
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.TH (testGroupGenerator)

import           Bcc.Api
import           Bcc.Api.Orphans ()
import           Gen.Bcc.Api (genAurumGenesis)
import           Gen.Bcc.Api.Typed

{- HLINT ignore "Use camelCase" -}

prop_json_roundtrip_aurum_genesis :: Property
prop_json_roundtrip_aurum_genesis = H.property $ do
  genesis <- forAll genAurumGenesis
  tripping genesis encode eitherDecode

prop_json_roundtrip_utxo :: Property
prop_json_roundtrip_utxo = H.property $ do
  utxo <- forAll $ genUTxO AurumEra
  tripping utxo encode eitherDecode

prop_json_roundtrip_txoutvalue :: Property
prop_json_roundtrip_txoutvalue = H.property $ do
  oVal <- forAll $ genTxOutValue AurumEra
  tripping oVal encode eitherDecode


prop_json_roundtrip_eraInMode :: Property
prop_json_roundtrip_eraInMode = H.property $ do
  H.assert $ parseEither rountripEraInModeParser ColeEraInColeMode == Right ColeEraInColeMode
  H.assert $ parseEither rountripEraInModeParser SophieEraInSophieMode == Right SophieEraInSophieMode
  H.assert $ parseEither rountripEraInModeParser ColeEraInBccMode == Right ColeEraInBccMode
  H.assert $ parseEither rountripEraInModeParser SophieEraInBccMode == Right SophieEraInBccMode
  H.assert $ parseEither rountripEraInModeParser EvieEraInBccMode == Right EvieEraInBccMode
  H.assert $ parseEither rountripEraInModeParser JenEraInBccMode == Right JenEraInBccMode
  H.assert $ parseEither rountripEraInModeParser AurumEraInBccMode == Right AurumEraInBccMode

  where
    -- Defined this way instead of using 'tripping' in order to warn the
    -- developer if there's ever a new constructor in 'EraInMode' and we would
    -- need to add a new 'FromJSON' instance.
    rountripEraInModeParser :: EraInMode era mode -> Parser (EraInMode era mode)
    rountripEraInModeParser = \case
      ColeEraInColeMode -> parseJSON $ toJSON ColeEraInColeMode
      SophieEraInSophieMode -> parseJSON $ toJSON SophieEraInSophieMode
      ColeEraInBccMode -> parseJSON $ toJSON ColeEraInBccMode
      SophieEraInBccMode -> parseJSON $ toJSON SophieEraInBccMode
      EvieEraInBccMode -> parseJSON $ toJSON EvieEraInBccMode
      JenEraInBccMode -> parseJSON $ toJSON JenEraInBccMode
      AurumEraInBccMode -> parseJSON $ toJSON AurumEraInBccMode


tests :: TestTree
tests = $testGroupGenerator
