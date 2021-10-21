{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}


-- | Transaction bodies
--
module Bcc.Api.TxBody (
    parseTxId,
    -- * Transaction bodies
    TxBody(.., TxBody),
    makeTransactionBody,
    TxBodyContent(..),
    TxBodyError(..),
    TxBodyScriptData(..),
    TxScriptValidity(..),
    TxScriptValiditySupportedInEra(..),

    ScriptValidity(..),
    scriptValidityToIsValid,
    isValidToScriptValidity,
    scriptValidityToTxScriptValidity,
    txScriptValidityToIsValid,
    txScriptValidityToScriptValidity,

    -- * Transaction Ids
    TxId(..),
    getTxId,
    getTxIdSophie,

    -- * Transaction inputs
    TxIn(..),
    TxIx(..),
    genesisUTxOPseudoTxIn,

    -- * Transaction outputs
    CtxTx, CtxUTxO,
    TxOut(..),
    TxOutValue(..),
    TxOutDatum(TxOutDatumNone, TxOutDatumHash, TxOutDatum),
    toCtxUTxOTxOut,
    entropicToTxOutValue,
    prettyRenderTxOut,
    txOutValueToEntropic,
    txOutValueToValue,
    parseHashScriptData,
    TxOutInAnyEra(..),
    txOutInAnyEra,

    -- * Other transaction body types
    TxInsCollateral(..),
    TxFee(..),
    TxValidityLowerBound(..),
    TxValidityUpperBound(..),
    TxMetadataInEra(..),
    TxAuxScripts(..),
    TxExtraKeyWitnesses(..),
    TxWithdrawals(..),
    TxCertificates(..),
    TxUpdateProposal(..),
    TxMintValue(..),

    -- ** Building vs viewing transactions
    BuildTxWith(..),
    BuildTx,
    ViewTx,

    -- * Era-dependent transaction body features
    CollateralSupportedInEra(..),
    MultiAssetSupportedInEra(..),
    OnlyDafiSupportedInEra(..),
    TxFeesExplicitInEra(..),
    TxFeesImplicitInEra(..),
    ValidityUpperBoundSupportedInEra(..),
    ValidityNoUpperBoundSupportedInEra(..),
    ValidityLowerBoundSupportedInEra(..),
    TxMetadataSupportedInEra(..),
    AuxScriptsSupportedInEra(..),
    TxExtraKeyWitnessesSupportedInEra(..),
    ScriptDataSupportedInEra(..),
    WithdrawalsSupportedInEra(..),
    CertificatesSupportedInEra(..),
    UpdateProposalSupportedInEra(..),

    -- ** Feature availability functions
    collateralSupportedInEra,
    multiAssetSupportedInEra,
    txFeesExplicitInEra,
    validityUpperBoundSupportedInEra,
    validityNoUpperBoundSupportedInEra,
    validityLowerBoundSupportedInEra,
    txMetadataSupportedInEra,
    auxScriptsSupportedInEra,
    extraKeyWitnessesSupportedInEra,
    scriptDataSupportedInEra,
    withdrawalsSupportedInEra,
    certificatesSupportedInEra,
    updateProposalSupportedInEra,
    txScriptValiditySupportedInSophieBasedEra,
    txScriptValiditySupportedInBccEra,

    -- * Inspecting 'ScriptWitness'es
    AnyScriptWitness(..),
    ScriptWitnessIndex(..),
    renderScriptWitnessIndex,
    collectTxBodyScriptWitnesses,
    mapTxScriptWitnesses,

    -- * Internal conversion functions & types
    toSophieTxId,
    toSophieTxIn,
    toSophieTxOut,
    toSophieTxOutAny,
    fromSophieTxId,
    fromSophieTxIn,
    fromSophieTxOut,
    toAurumRdmrPtr,
    fromAurumRdmrPtr,
    fromColeTxIn,
    fromLedgerTxOuts,
    renderTxIn,

    -- * Data family instances
    AsType(AsTxId, AsTxBody, AsColeTxBody, AsSophieTxBody, AsJenTxBody),
  ) where

import           Prelude

import           Control.Monad (guard)
import           Data.Aeson (object, withObject, withText, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (ToJSONKey (..), toJSONKeyText)
import qualified Data.Aeson.Types as Aeson
import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (toList)
import           Data.Function (on)
import qualified Data.HashMap.Strict as HMS
import           Data.List (intercalate, sortBy)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, maybeToList)
import           Data.Scientific (toBoundedInteger)
import qualified Data.Sequence.Strict as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word64)
import           GHC.Generics
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Language as Parsec
import qualified Text.Parsec.String as Parsec
import qualified Text.Parsec.Token as Parsec

import           Bcc.Binary (Annotated (..), reAnnotate, recoverBytes)
import qualified Bcc.Binary as CBOR
import qualified Bcc.Crypto.Hash.Class as Crypto
import qualified Bcc.Ledger.Serialization as CBOR (decodeNullMaybe, encodeNullMaybe)
import           Bcc.Slotting.Slot (SlotNo (..))

import qualified Bcc.Chain.Common as Cole
import qualified Bcc.Chain.UTxO as Cole
import qualified Bcc.Crypto.Hashing as Cole

import qualified Bcc.Ledger.Address as Sophie
import qualified Bcc.Ledger.AuxiliaryData as Ledger (hashAuxiliaryData)
import           Bcc.Ledger.BaseTypes (StrictMaybe (..), maybeToStrictMaybe)
import qualified Bcc.Ledger.Core as Core
import qualified Bcc.Ledger.Core as Ledger
import qualified Bcc.Ledger.Credential as Sophie
import qualified Bcc.Ledger.Era as Ledger
import qualified Bcc.Ledger.Keys as Sophie
import qualified Bcc.Ledger.SafeHash as SafeHash
import qualified Bcc.Ledger.Sophie.Constraints as Ledger

import qualified Bcc.Ledger.Sophie.Genesis as Sophie
import qualified Bcc.Ledger.Sophie.Metadata as Sophie
import qualified Bcc.Ledger.Sophie.Tx as Sophie
import qualified Bcc.Ledger.Sophie.TxBody as Sophie
import qualified Bcc.Ledger.TxIn as Ledger

import qualified Bcc.Ledger.Jen.Value as Jen
import qualified Bcc.Ledger.SophieMA.AuxiliaryData as Evie
import qualified Bcc.Ledger.SophieMA.AuxiliaryData as Jen
import qualified Bcc.Ledger.SophieMA.TxBody as Evie
import qualified Bcc.Ledger.SophieMA.TxBody as Jen
import           Bcc.Ledger.Val (isZero)

import qualified Bcc.Ledger.Aurum as Aurum
import qualified Bcc.Ledger.Aurum.Data as Aurum
import qualified Bcc.Ledger.Aurum.Language as Aurum
import qualified Bcc.Ledger.Aurum.Scripts as Aurum
import qualified Bcc.Ledger.Aurum.Tx as Aurum
import qualified Bcc.Ledger.Aurum.TxBody as Aurum
import qualified Bcc.Ledger.Aurum.TxWitness as Aurum

import           Shardagnostic.Consensus.Sophie.Eras (StandardEvie, StandardAurum, StandardJen,
                   StandardSophie)

import           Bcc.Api.Address
import           Bcc.Api.Certificate
import           Bcc.Api.Eras
import           Bcc.Api.Error
import           Bcc.Api.HasTypeProxy
import           Bcc.Api.Hash
import           Bcc.Api.KeysCole
import           Bcc.Api.KeysSophie
import           Bcc.Api.NetworkId
import           Bcc.Api.ProtocolParameters
import           Bcc.Api.Script
import           Bcc.Api.ScriptData
import           Bcc.Api.SerialiseCBOR
import           Bcc.Api.SerialiseJSON
import           Bcc.Api.SerialiseRaw
import           Bcc.Api.SerialiseTextEnvelope
import           Bcc.Api.SerialiseUsing
import           Bcc.Api.TxMetadata
import           Bcc.Api.Utils
import           Bcc.Api.Value
import           Bcc.Api.ValueParser
import           Bcc.Ledger.Crypto (StandardCrypto)

{- HLINT ignore "Redundant flip" -}
{- HLINT ignore "Use section" -}

-- | Indicates whether a script is expected to fail or pass validation.
data ScriptValidity
  = ScriptInvalid -- ^ Script is expected to fail validation.
                  -- Transactions marked as such can include scripts that fail validation.
                  -- Such transactions may be submitted to the chain, in which case the
                  -- collateral will be taken upon on chain script validation failure.

  | ScriptValid   -- ^ Script is expected to pass validation.
                  -- Transactions marked as such cannot include scripts that fail validation.

  deriving (Eq, Show)

instance ToCBOR ScriptValidity where
  toCBOR = toCBOR . scriptValidityToIsValid

instance FromCBOR ScriptValidity where
  fromCBOR = isValidToScriptValidity <$> fromCBOR

scriptValidityToIsValid :: ScriptValidity -> Aurum.IsValid
scriptValidityToIsValid ScriptInvalid = Aurum.IsValid False
scriptValidityToIsValid ScriptValid = Aurum.IsValid True

isValidToScriptValidity :: Aurum.IsValid -> ScriptValidity
isValidToScriptValidity (Aurum.IsValid False) = ScriptInvalid
isValidToScriptValidity (Aurum.IsValid True) = ScriptValid

-- | A representation of whether the era supports tx script validity.
--
-- The Jen and subsequent eras support script validity.
--
data TxScriptValidity era where
  TxScriptValidityNone :: TxScriptValidity era

  -- | Tx script validity is supported in transactions in the 'Aurum' era onwards.
  TxScriptValidity
    :: TxScriptValiditySupportedInEra era
    -> ScriptValidity
    -> TxScriptValidity era

deriving instance Eq   (TxScriptValiditySupportedInEra era)
deriving instance Show (TxScriptValiditySupportedInEra era)

data TxScriptValiditySupportedInEra era where
  TxScriptValiditySupportedInAurumEra :: TxScriptValiditySupportedInEra AurumEra

deriving instance Eq   (TxScriptValidity era)
deriving instance Show (TxScriptValidity era)

txScriptValiditySupportedInBccEra :: BccEra era -> Maybe (TxScriptValiditySupportedInEra era)
txScriptValiditySupportedInBccEra ColeEra   = Nothing
txScriptValiditySupportedInBccEra SophieEra = Nothing
txScriptValiditySupportedInBccEra EvieEra = Nothing
txScriptValiditySupportedInBccEra JenEra    = Nothing
txScriptValiditySupportedInBccEra AurumEra  = Just TxScriptValiditySupportedInAurumEra

txScriptValiditySupportedInSophieBasedEra :: SophieBasedEra era -> Maybe (TxScriptValiditySupportedInEra era)
txScriptValiditySupportedInSophieBasedEra SophieBasedEraSophie = Nothing
txScriptValiditySupportedInSophieBasedEra SophieBasedEraEvie = Nothing
txScriptValiditySupportedInSophieBasedEra SophieBasedEraJen    = Nothing
txScriptValiditySupportedInSophieBasedEra SophieBasedEraAurum  = Just TxScriptValiditySupportedInAurumEra

txScriptValidityToScriptValidity :: TxScriptValidity era -> ScriptValidity
txScriptValidityToScriptValidity TxScriptValidityNone = ScriptValid
txScriptValidityToScriptValidity (TxScriptValidity _ scriptValidity) = scriptValidity

scriptValidityToTxScriptValidity :: SophieBasedEra era -> ScriptValidity -> TxScriptValidity era
scriptValidityToTxScriptValidity era scriptValidity = case txScriptValiditySupportedInSophieBasedEra era of
  Nothing -> TxScriptValidityNone
  Just witness -> TxScriptValidity witness scriptValidity

txScriptValidityToIsValid :: TxScriptValidity era -> Aurum.IsValid
txScriptValidityToIsValid = scriptValidityToIsValid . txScriptValidityToScriptValidity

-- ----------------------------------------------------------------------------
-- Transaction Ids
--

newtype TxId = TxId (Sophie.Hash StandardCrypto Sophie.EraIndependentTxBody)
  -- We use the Sophie representation and convert to/from the Cole one
  deriving stock (Eq, Ord)
  deriving (Show, IsString)         via UsingRawBytesHex TxId
  deriving (ToJSON, FromJSON)       via UsingRawBytesHex TxId
  deriving (ToJSONKey, FromJSONKey) via UsingRawBytesHex TxId

instance HasTypeProxy TxId where
    data AsType TxId = AsTxId
    proxyToAsType _ = AsTxId

instance SerialiseAsRawBytes TxId where
    serialiseToRawBytes (TxId h) = Crypto.hashToBytes h
    deserialiseFromRawBytes AsTxId bs = TxId <$> Crypto.hashFromBytes bs

toColeTxId :: TxId -> Cole.TxId
toColeTxId (TxId h) =
    Cole.unsafeHashFromBytes (Crypto.hashToBytes h)

toSophieTxId :: TxId -> Ledger.TxId StandardCrypto
toSophieTxId (TxId h) =
    Ledger.TxId (SafeHash.unsafeMakeSafeHash (Crypto.castHash h))

fromSophieTxId :: Ledger.TxId StandardCrypto -> TxId
fromSophieTxId (Ledger.TxId h) =
    TxId (Crypto.castHash (SafeHash.extractHash h))

-- | Calculate the transaction identifier for a 'TxBody'.
--
getTxId :: forall era. TxBody era -> TxId
getTxId (ColeTxBody tx) =
    TxId
  . fromMaybe impossible
  . Crypto.hashFromBytesShort
  . Cole.abstractHashToShort
  . Cole.hashDecoded
  $ tx
  where
    impossible =
      error "getTxId: cole and sophie hash sizes do not match"

getTxId (SophieTxBody era tx _ _ _ _) =
  obtainConstraints era $ getTxIdSophie era tx
 where
  obtainConstraints
    :: SophieBasedEra era
    -> (( Ledger.Crypto (SophieLedgerEra era) ~ StandardCrypto
        , Ledger.UsesTxBody (SophieLedgerEra era)
        ) => a)
    -> a
  obtainConstraints SophieBasedEraSophie f = f
  obtainConstraints SophieBasedEraEvie f = f
  obtainConstraints SophieBasedEraJen    f = f
  obtainConstraints SophieBasedEraAurum  f = f

getTxIdSophie
  :: Ledger.Crypto (SophieLedgerEra era) ~ StandardCrypto
  => Ledger.UsesTxBody (SophieLedgerEra era)
  => SophieBasedEra era -> Ledger.TxBody (SophieLedgerEra era) -> TxId
getTxIdSophie _ tx =
    TxId
  . Crypto.castHash
  . (\(Ledger.TxId txhash) -> SafeHash.extractHash txhash)
  $ Ledger.txid tx


-- ----------------------------------------------------------------------------
-- Transaction inputs
--

data TxIn = TxIn TxId TxIx
  deriving (Eq, Ord, Show)

instance ToJSON TxIn where
  toJSON txIn = Aeson.String $ renderTxIn txIn

instance ToJSONKey TxIn where
  toJSONKey = toJSONKeyText renderTxIn

instance FromJSON TxIn where
  parseJSON = withText "TxIn" $ \txinStr -> runParsecParser parseTxIn txinStr

parseTxId :: Parsec.Parser TxId
parseTxId = do
  str <- Parsec.many1 Parsec.hexDigit Parsec.<?> "transaction id (hexadecimal)"
  case deserialiseFromRawBytesHex AsTxId (BSC.pack str) of
    Just addr -> return addr
    Nothing -> fail $ "Incorrect transaction id format:: " ++ show str

parseTxIn :: Parsec.Parser TxIn
parseTxIn = TxIn <$> parseTxId <*> (Parsec.char '#' *> parseTxIx)

parseTxIx :: Parsec.Parser TxIx
parseTxIx = TxIx . fromIntegral <$> decimal

decimal :: Parsec.Parser Integer
Parsec.TokenParser { Parsec.decimal = decimal } = Parsec.haskell


renderTxIn :: TxIn -> Text
renderTxIn (TxIn txId (TxIx ix)) =
  serialiseToRawBytesHexText txId <> "#" <> Text.pack (show ix)


newtype TxIx = TxIx Word
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)
  deriving newtype (ToJSON, FromJSON)

fromColeTxIn :: Cole.TxIn -> TxIn
fromColeTxIn (Cole.TxInUtxo txId index) =
  let shortBs = Cole.abstractHashToShort txId
      mApiHash = Crypto.hashFromBytesShort shortBs
  in case mApiHash of
       Just apiHash -> TxIn (TxId apiHash) (TxIx . fromIntegral $ toInteger index)
       Nothing -> error $ "Error converting Cole era TxId: " <> show txId

toColeTxIn :: TxIn -> Cole.TxIn
toColeTxIn (TxIn txid (TxIx txix)) =
    Cole.TxInUtxo (toColeTxId txid) (fromIntegral txix)

toSophieTxIn :: TxIn -> Ledger.TxIn StandardCrypto
toSophieTxIn (TxIn txid (TxIx txix)) =
    Ledger.TxIn (toSophieTxId txid) (fromIntegral txix)

fromSophieTxIn :: Ledger.TxIn StandardCrypto -> TxIn
fromSophieTxIn (Ledger.TxIn txid txix) =
    TxIn (fromSophieTxId txid) (TxIx (fromIntegral txix))


-- ----------------------------------------------------------------------------
-- Transaction outputs
--

-- | The context is a transaction body
data CtxTx
-- | The context is the UTxO
data CtxUTxO

data TxOut ctx era = TxOut (AddressInEra   era)
                           (TxOutValue     era)
                           (TxOutDatum ctx era)

deriving instance Eq   (TxOut ctx era)
deriving instance Show (TxOut ctx era)

data TxOutInAnyEra where
     TxOutInAnyEra :: BccEra era
                   -> TxOut CtxTx era
                   -> TxOutInAnyEra

deriving instance Show TxOutInAnyEra

-- | Convenience constructor for 'TxOutInAnyEra'
txOutInAnyEra :: IsBccEra era => TxOut CtxTx era -> TxOutInAnyEra
txOutInAnyEra = TxOutInAnyEra bccEra

toCtxUTxOTxOut :: TxOut CtxTx  era -> TxOut CtxUTxO era
toCtxUTxOTxOut (TxOut addr val d) =
  let dat = case d of
              TxOutDatumNone -> TxOutDatumNone
              TxOutDatumHash s h -> TxOutDatumHash s h
              TxOutDatum' s h _ -> TxOutDatumHash s h
  in TxOut addr val dat

instance IsBccEra era => ToJSON (TxOut ctx era) where
  toJSON (TxOut addr val TxOutDatumNone) =
    object [ "address" .= addr
           , "value"   .= val
           ]
  toJSON (TxOut addr val (TxOutDatumHash _ h)) =
    object [ "address"   .= addr
           , "value"     .= val
           , "datumhash" .= h
           ]
  toJSON (TxOut addr val (TxOutDatum' _ h d)) =
    object [ "address"   .= addr
           , "value"     .= val
           , "datumhash" .= h
           , "datum"     .= scriptDataToJson ScriptDataJsonDetailedSchema d
           ]

instance (IsSophieBasedEra era, IsBccEra era)
  => FromJSON (TxOut CtxTx era) where
      parseJSON = withObject "TxOut" $ \o -> do
        addr <- o .: "address"
        val <- o .: "value"
        case scriptDataSupportedInEra bccEra of
          Just supported -> do
            mData <- o .:? "datum"
            mDatumHash <- o .:? "datumhash"
            case (mData, mDatumHash) of
              (Nothing, Nothing) ->
                pure $ TxOut addr val TxOutDatumNone
              (Nothing, Just (Aeson.String dHashTxt))  -> do
                dh <- runParsecParser parseHashScriptData dHashTxt
                pure $ TxOut addr val $ TxOutDatumHash supported dh
              (Just sDataVal, Just (Aeson.String dHashTxt)) ->
                case scriptDataFromJson ScriptDataJsonDetailedSchema sDataVal of
                  Left err ->
                    fail $ "Error parsing TxOut JSON: " <> displayError err
                  Right sData -> do
                    dHash <- runParsecParser parseHashScriptData dHashTxt
                    pure . TxOut addr val $ TxOutDatum' supported dHash sData
              (mDatVal, wrongDatumHashFormat) ->
                fail $ "Error parsing TxOut's datum hash/data: " <>
                       "\nData value:" <> show mDatVal <>
                       "\nDatumHash: " <> show wrongDatumHashFormat

          Nothing -> pure $ TxOut addr val TxOutDatumNone

instance (IsSophieBasedEra era, IsBccEra era)
  => FromJSON (TxOut CtxUTxO era) where
      parseJSON = withObject "TxOut" $ \o -> do
        addr <- o .: "address"
        val <- o .: "value"
        case scriptDataSupportedInEra bccEra of
          Just supported -> do
            mDatumHash <- o .:? "datumhash"
            case mDatumHash of
              Nothing ->
                pure $ TxOut addr val TxOutDatumNone
              Just (Aeson.String dHashTxt)  -> do
                dh <- runParsecParser parseHashScriptData dHashTxt
                pure $ TxOut addr val $ TxOutDatumHash supported dh
              Just wrongDatumHashFormat ->
                fail $ "Error parsing TxOut's datum hash: " <>
                       "\nDatumHash: " <> show wrongDatumHashFormat
          Nothing -> pure $ TxOut addr val TxOutDatumNone

fromColeTxOut :: Cole.TxOut -> TxOut ctx ColeEra
fromColeTxOut (Cole.TxOut addr value) =
  TxOut
    (AddressInEra ColeAddressInAnyEra (ColeAddress addr))
    (TxOutDafiOnly DafiOnlyInColeEra (fromColeEntropic value))
     TxOutDatumNone


toColeTxOut :: TxOut ctx ColeEra -> Maybe Cole.TxOut
toColeTxOut (TxOut (AddressInEra ColeAddressInAnyEra (ColeAddress addr))
                    (TxOutDafiOnly DafiOnlyInColeEra value) _) =
    Cole.TxOut addr <$> toColeEntropic value

toColeTxOut (TxOut (AddressInEra ColeAddressInAnyEra (ColeAddress _))
                    (TxOutValue era _) _) = case era of {}

toColeTxOut (TxOut (AddressInEra (SophieAddressInEra era) SophieAddress{})
                    _ _) = case era of {}


toSophieTxOut :: forall era ledgerera.
                  SophieLedgerEra era ~ ledgerera
               => SophieBasedEra era
               -> TxOut CtxUTxO era
               -> Ledger.TxOut ledgerera
toSophieTxOut era (TxOut _ (TxOutDafiOnly DafiOnlyInColeEra _) _) =
    case era of {}

toSophieTxOut _ (TxOut addr (TxOutDafiOnly DafiOnlyInSophieEra value) _) =
    Sophie.TxOut (toSophieAddr addr) (toSophieEntropic value)

toSophieTxOut _ (TxOut addr (TxOutDafiOnly DafiOnlyInEvieEra value) _) =
    Sophie.TxOut (toSophieAddr addr) (toSophieEntropic value)

toSophieTxOut _ (TxOut addr (TxOutValue MultiAssetInJenEra value) _) =
    Sophie.TxOut (toSophieAddr addr) (toJenValue value)

toSophieTxOut _ (TxOut addr (TxOutValue MultiAssetInAurumEra value) txoutdata) =
    Aurum.TxOut (toSophieAddr addr) (toJenValue value)
                 (toAurumTxOutDataHash txoutdata)

fromSophieTxOut :: SophieLedgerEra era ~ ledgerera
                 => SophieBasedEra era
                 -> Core.TxOut ledgerera
                 -> TxOut ctx era
fromSophieTxOut era ledgerTxOut =
  case era of
    SophieBasedEraSophie ->
        TxOut (fromSophieAddr addr)
              (TxOutDafiOnly DafiOnlyInSophieEra
                            (fromSophieEntropic value))
               TxOutDatumNone
      where
        Sophie.TxOut addr value = ledgerTxOut

    SophieBasedEraEvie ->
        TxOut (fromSophieAddr addr)
              (TxOutDafiOnly DafiOnlyInEvieEra
                            (fromSophieEntropic value))
               TxOutDatumNone
      where
        Sophie.TxOut addr value = ledgerTxOut

    SophieBasedEraJen ->
        TxOut (fromSophieAddr addr)
              (TxOutValue MultiAssetInJenEra
                          (fromJenValue value))
               TxOutDatumNone
      where
        Sophie.TxOut addr value = ledgerTxOut

    SophieBasedEraAurum ->
       TxOut (fromSophieAddr addr)
             (TxOutValue MultiAssetInAurumEra
                         (fromJenValue value))
             (fromAurumTxOutDataHash ScriptDataInAurumEra datahash)
      where
        Aurum.TxOut addr value datahash = ledgerTxOut

toAurumTxOutDataHash :: TxOutDatum CtxUTxO era
                      -> StrictMaybe (Aurum.DataHash StandardCrypto)
toAurumTxOutDataHash  TxOutDatumNone                        = SNothing
toAurumTxOutDataHash (TxOutDatumHash _ (ScriptDataHash dh)) = SJust dh

fromAurumTxOutDataHash :: ScriptDataSupportedInEra era
                        -> StrictMaybe (Aurum.DataHash StandardCrypto)
                        -> TxOutDatum ctx era
fromAurumTxOutDataHash _    SNothing  = TxOutDatumNone
fromAurumTxOutDataHash era (SJust dh) = TxOutDatumHash era (ScriptDataHash dh)


-- ----------------------------------------------------------------------------
-- Era-dependent transaction body features
--

-- | A representation of whether the era supports transactions with inputs used
-- only for collateral for script fees.
--
-- The Aurum and subsequent eras support collateral inputs.
--
data CollateralSupportedInEra era where

     CollateralInAurumEra :: CollateralSupportedInEra AurumEra

deriving instance Eq   (CollateralSupportedInEra era)
deriving instance Show (CollateralSupportedInEra era)

collateralSupportedInEra :: BccEra era
                         -> Maybe (CollateralSupportedInEra era)
collateralSupportedInEra ColeEra   = Nothing
collateralSupportedInEra SophieEra = Nothing
collateralSupportedInEra EvieEra = Nothing
collateralSupportedInEra JenEra    = Nothing
collateralSupportedInEra AurumEra  = Just CollateralInAurumEra


-- | A representation of whether the era supports multi-asset transactions.
--
-- The Jen and subsequent eras support multi-asset transactions.
--
-- The negation of this is 'OnlyDafiSupportedInEra'.
--
data MultiAssetSupportedInEra era where

     -- | Multi-asset transactions are supported in the 'Jen' era.
     MultiAssetInJenEra :: MultiAssetSupportedInEra JenEra

     -- | Multi-asset transactions are supported in the 'Aurum' era.
     MultiAssetInAurumEra :: MultiAssetSupportedInEra AurumEra

deriving instance Eq   (MultiAssetSupportedInEra era)
deriving instance Show (MultiAssetSupportedInEra era)

instance ToJSON (MultiAssetSupportedInEra era) where
  toJSON = Aeson.String . Text.pack . show

-- | A representation of whether the era supports only dafi transactions.
--
-- Prior to the Jen era only dafi transactions are supported. Multi-assets are
-- supported from the Jen era onwards.
--
-- This is the negation of 'MultiAssetSupportedInEra'. It exists since we need
-- evidence to be positive.
--
data OnlyDafiSupportedInEra era where

     DafiOnlyInColeEra   :: OnlyDafiSupportedInEra ColeEra
     DafiOnlyInSophieEra :: OnlyDafiSupportedInEra SophieEra
     DafiOnlyInEvieEra :: OnlyDafiSupportedInEra EvieEra

deriving instance Eq   (OnlyDafiSupportedInEra era)
deriving instance Show (OnlyDafiSupportedInEra era)

multiAssetSupportedInEra :: BccEra era
                         -> Either (OnlyDafiSupportedInEra era)
                                   (MultiAssetSupportedInEra era)
multiAssetSupportedInEra ColeEra   = Left DafiOnlyInColeEra
multiAssetSupportedInEra SophieEra = Left DafiOnlyInSophieEra
multiAssetSupportedInEra EvieEra = Left DafiOnlyInEvieEra
multiAssetSupportedInEra JenEra    = Right MultiAssetInJenEra
multiAssetSupportedInEra AurumEra  = Right MultiAssetInAurumEra


-- | A representation of whether the era requires explicitly specified fees in
-- transactions.
--
-- The Cole era tx fees are implicit (as the difference bettween the sum of
-- outputs and sum of inputs), but all later eras the fees are specified in the
-- transaction explicitly.
--
data TxFeesExplicitInEra era where

     TxFeesExplicitInSophieEra :: TxFeesExplicitInEra SophieEra
     TxFeesExplicitInEvieEra :: TxFeesExplicitInEra EvieEra
     TxFeesExplicitInJenEra    :: TxFeesExplicitInEra JenEra
     TxFeesExplicitInAurumEra  :: TxFeesExplicitInEra AurumEra

deriving instance Eq   (TxFeesExplicitInEra era)
deriving instance Show (TxFeesExplicitInEra era)

-- | A representation of whether the era requires implicitly specified fees in
-- transactions.
--
-- This is the negation of 'TxFeesExplicitInEra'.
--
data TxFeesImplicitInEra era where
     TxFeesImplicitInColeEra :: TxFeesImplicitInEra ColeEra

deriving instance Eq   (TxFeesImplicitInEra era)
deriving instance Show (TxFeesImplicitInEra era)

txFeesExplicitInEra :: BccEra era
                    -> Either (TxFeesImplicitInEra era)
                              (TxFeesExplicitInEra era)
txFeesExplicitInEra ColeEra   = Left  TxFeesImplicitInColeEra
txFeesExplicitInEra SophieEra = Right TxFeesExplicitInSophieEra
txFeesExplicitInEra EvieEra = Right TxFeesExplicitInEvieEra
txFeesExplicitInEra JenEra    = Right TxFeesExplicitInJenEra
txFeesExplicitInEra AurumEra  = Right TxFeesExplicitInAurumEra


-- | A representation of whether the era supports transactions with an upper
-- bound on the range of slots in which they are valid.
--
-- The Sophie and subsequent eras support an upper bound on the validity
-- range. In the Sophie era specifically it is actually required. It is
-- optional in later eras.
--
data ValidityUpperBoundSupportedInEra era where

     ValidityUpperBoundInSophieEra :: ValidityUpperBoundSupportedInEra SophieEra
     ValidityUpperBoundInEvieEra :: ValidityUpperBoundSupportedInEra EvieEra
     ValidityUpperBoundInJenEra    :: ValidityUpperBoundSupportedInEra JenEra
     ValidityUpperBoundInAurumEra  :: ValidityUpperBoundSupportedInEra AurumEra

deriving instance Eq   (ValidityUpperBoundSupportedInEra era)
deriving instance Show (ValidityUpperBoundSupportedInEra era)

validityUpperBoundSupportedInEra :: BccEra era
                                 -> Maybe (ValidityUpperBoundSupportedInEra era)
validityUpperBoundSupportedInEra ColeEra   = Nothing
validityUpperBoundSupportedInEra SophieEra = Just ValidityUpperBoundInSophieEra
validityUpperBoundSupportedInEra EvieEra = Just ValidityUpperBoundInEvieEra
validityUpperBoundSupportedInEra JenEra    = Just ValidityUpperBoundInJenEra
validityUpperBoundSupportedInEra AurumEra  = Just ValidityUpperBoundInAurumEra


-- | A representation of whether the era supports transactions having /no/
-- upper bound on the range of slots in which they are valid.
--
-- Note that the 'SophieEra' /does not support/ omitting a validity upper
-- bound. It was introduced as a /required/ field in Sophie and then made
-- optional in Evie and subsequent eras.
--
-- The Cole era supports this by virtue of the fact that it does not support
-- validity ranges at all.
--
data ValidityNoUpperBoundSupportedInEra era where

     ValidityNoUpperBoundInColeEra   :: ValidityNoUpperBoundSupportedInEra ColeEra
     ValidityNoUpperBoundInEvieEra :: ValidityNoUpperBoundSupportedInEra EvieEra
     ValidityNoUpperBoundInJenEra    :: ValidityNoUpperBoundSupportedInEra JenEra
     ValidityNoUpperBoundInAurumEra  :: ValidityNoUpperBoundSupportedInEra AurumEra

deriving instance Eq   (ValidityNoUpperBoundSupportedInEra era)
deriving instance Show (ValidityNoUpperBoundSupportedInEra era)

validityNoUpperBoundSupportedInEra :: BccEra era
                                   -> Maybe (ValidityNoUpperBoundSupportedInEra era)
validityNoUpperBoundSupportedInEra ColeEra   = Just ValidityNoUpperBoundInColeEra
validityNoUpperBoundSupportedInEra SophieEra = Nothing
validityNoUpperBoundSupportedInEra EvieEra = Just ValidityNoUpperBoundInEvieEra
validityNoUpperBoundSupportedInEra JenEra    = Just ValidityNoUpperBoundInJenEra
validityNoUpperBoundSupportedInEra AurumEra  = Just ValidityNoUpperBoundInAurumEra


-- | A representation of whether the era supports transactions with a lower
-- bound on the range of slots in which they are valid.
--
-- The Evie and subsequent eras support an optional lower bound on the
-- validity range. No equivalent of 'ValidityNoUpperBoundSupportedInEra' is
-- needed since all eras support having no lower bound.
--
data ValidityLowerBoundSupportedInEra era where

     ValidityLowerBoundInEvieEra :: ValidityLowerBoundSupportedInEra EvieEra
     ValidityLowerBoundInJenEra    :: ValidityLowerBoundSupportedInEra JenEra
     ValidityLowerBoundInAurumEra  :: ValidityLowerBoundSupportedInEra AurumEra

deriving instance Eq   (ValidityLowerBoundSupportedInEra era)
deriving instance Show (ValidityLowerBoundSupportedInEra era)

validityLowerBoundSupportedInEra :: BccEra era
                                 -> Maybe (ValidityLowerBoundSupportedInEra era)
validityLowerBoundSupportedInEra ColeEra   = Nothing
validityLowerBoundSupportedInEra SophieEra = Nothing
validityLowerBoundSupportedInEra EvieEra = Just ValidityLowerBoundInEvieEra
validityLowerBoundSupportedInEra JenEra    = Just ValidityLowerBoundInJenEra
validityLowerBoundSupportedInEra AurumEra  = Just ValidityLowerBoundInAurumEra

-- | A representation of whether the era supports transaction metadata.
--
-- Transaction metadata is supported from the Sophie era onwards.
--
data TxMetadataSupportedInEra era where

     TxMetadataInSophieEra :: TxMetadataSupportedInEra SophieEra
     TxMetadataInEvieEra :: TxMetadataSupportedInEra EvieEra
     TxMetadataInJenEra    :: TxMetadataSupportedInEra JenEra
     TxMetadataInAurumEra  :: TxMetadataSupportedInEra AurumEra

deriving instance Eq   (TxMetadataSupportedInEra era)
deriving instance Show (TxMetadataSupportedInEra era)

txMetadataSupportedInEra :: BccEra era
                         -> Maybe (TxMetadataSupportedInEra era)
txMetadataSupportedInEra ColeEra   = Nothing
txMetadataSupportedInEra SophieEra = Just TxMetadataInSophieEra
txMetadataSupportedInEra EvieEra = Just TxMetadataInEvieEra
txMetadataSupportedInEra JenEra    = Just TxMetadataInJenEra
txMetadataSupportedInEra AurumEra  = Just TxMetadataInAurumEra


-- | A representation of whether the era supports auxiliary scripts in
-- transactions.
--
-- Auxiliary scripts are supported from the Evie era onwards.
--
data AuxScriptsSupportedInEra era where

     AuxScriptsInEvieEra :: AuxScriptsSupportedInEra EvieEra
     AuxScriptsInJenEra    :: AuxScriptsSupportedInEra JenEra
     AuxScriptsInAurumEra  :: AuxScriptsSupportedInEra AurumEra

deriving instance Eq   (AuxScriptsSupportedInEra era)
deriving instance Show (AuxScriptsSupportedInEra era)

auxScriptsSupportedInEra :: BccEra era
                         -> Maybe (AuxScriptsSupportedInEra era)
auxScriptsSupportedInEra ColeEra   = Nothing
auxScriptsSupportedInEra SophieEra = Nothing
auxScriptsSupportedInEra EvieEra = Just AuxScriptsInEvieEra
auxScriptsSupportedInEra JenEra    = Just AuxScriptsInJenEra
auxScriptsSupportedInEra AurumEra  = Just AuxScriptsInAurumEra


-- | A representation of whether the era supports transactions that specify
-- in the body that they need extra key witnesses, and where this fact is
-- visible to scripts.
--
-- Extra key witnesses visible to scripts are supported from the Aurum era
-- onwards.
--
data TxExtraKeyWitnessesSupportedInEra era where

     ExtraKeyWitnessesInAurumEra :: TxExtraKeyWitnessesSupportedInEra AurumEra


deriving instance Eq   (TxExtraKeyWitnessesSupportedInEra era)
deriving instance Show (TxExtraKeyWitnessesSupportedInEra era)

extraKeyWitnessesSupportedInEra :: BccEra era
                                -> Maybe (TxExtraKeyWitnessesSupportedInEra era)
extraKeyWitnessesSupportedInEra ColeEra   = Nothing
extraKeyWitnessesSupportedInEra SophieEra = Nothing
extraKeyWitnessesSupportedInEra EvieEra = Nothing
extraKeyWitnessesSupportedInEra JenEra    = Nothing
extraKeyWitnessesSupportedInEra AurumEra  = Just ExtraKeyWitnessesInAurumEra


-- | A representation of whether the era supports multi-asset transactions.
--
-- The Jen and subsequent eras support multi-asset transactions.
--
-- The negation of this is 'OnlyDafiSupportedInEra'.
--
data ScriptDataSupportedInEra era where

     -- | Script data is supported in transactions in the 'Aurum' era.
     ScriptDataInAurumEra :: ScriptDataSupportedInEra AurumEra

deriving instance Eq   (ScriptDataSupportedInEra era)
deriving instance Show (ScriptDataSupportedInEra era)

scriptDataSupportedInEra :: BccEra era
                         -> Maybe (ScriptDataSupportedInEra era)
scriptDataSupportedInEra ColeEra   = Nothing
scriptDataSupportedInEra SophieEra = Nothing
scriptDataSupportedInEra EvieEra = Nothing
scriptDataSupportedInEra JenEra    = Nothing
scriptDataSupportedInEra AurumEra  = Just ScriptDataInAurumEra


-- | A representation of whether the era supports withdrawals from reward
-- accounts.
--
-- The Sophie and subsequent eras support stake addresses, their associated
-- reward accounts and support for withdrawals from them.
--
data WithdrawalsSupportedInEra era where

     WithdrawalsInSophieEra :: WithdrawalsSupportedInEra SophieEra
     WithdrawalsInEvieEra :: WithdrawalsSupportedInEra EvieEra
     WithdrawalsInJenEra    :: WithdrawalsSupportedInEra JenEra
     WithdrawalsInAurumEra  :: WithdrawalsSupportedInEra AurumEra

deriving instance Eq   (WithdrawalsSupportedInEra era)
deriving instance Show (WithdrawalsSupportedInEra era)

withdrawalsSupportedInEra :: BccEra era
                          -> Maybe (WithdrawalsSupportedInEra era)
withdrawalsSupportedInEra ColeEra   = Nothing
withdrawalsSupportedInEra SophieEra = Just WithdrawalsInSophieEra
withdrawalsSupportedInEra EvieEra = Just WithdrawalsInEvieEra
withdrawalsSupportedInEra JenEra    = Just WithdrawalsInJenEra
withdrawalsSupportedInEra AurumEra  = Just WithdrawalsInAurumEra


-- | A representation of whether the era supports 'Certificate's embedded in
-- transactions.
--
-- The Sophie and subsequent eras support such certificates.
--
data CertificatesSupportedInEra era where

     CertificatesInSophieEra :: CertificatesSupportedInEra SophieEra
     CertificatesInEvieEra :: CertificatesSupportedInEra EvieEra
     CertificatesInJenEra    :: CertificatesSupportedInEra JenEra
     CertificatesInAurumEra  :: CertificatesSupportedInEra AurumEra

deriving instance Eq   (CertificatesSupportedInEra era)
deriving instance Show (CertificatesSupportedInEra era)

certificatesSupportedInEra :: BccEra era
                           -> Maybe (CertificatesSupportedInEra era)
certificatesSupportedInEra ColeEra   = Nothing
certificatesSupportedInEra SophieEra = Just CertificatesInSophieEra
certificatesSupportedInEra EvieEra = Just CertificatesInEvieEra
certificatesSupportedInEra JenEra    = Just CertificatesInJenEra
certificatesSupportedInEra AurumEra  = Just CertificatesInAurumEra


-- | A representation of whether the era supports 'UpdateProposal's embedded in
-- transactions.
--
-- The Sophie and subsequent eras support such update proposals. They Cole
-- era has a notion of an update proposal, but it is a standalone chain object
-- and not embedded in a transaction.
--
data UpdateProposalSupportedInEra era where

     UpdateProposalInSophieEra :: UpdateProposalSupportedInEra SophieEra
     UpdateProposalInEvieEra :: UpdateProposalSupportedInEra EvieEra
     UpdateProposalInJenEra    :: UpdateProposalSupportedInEra JenEra
     UpdateProposalInAurumEra  :: UpdateProposalSupportedInEra AurumEra

deriving instance Eq   (UpdateProposalSupportedInEra era)
deriving instance Show (UpdateProposalSupportedInEra era)

updateProposalSupportedInEra :: BccEra era
                             -> Maybe (UpdateProposalSupportedInEra era)
updateProposalSupportedInEra ColeEra   = Nothing
updateProposalSupportedInEra SophieEra = Just UpdateProposalInSophieEra
updateProposalSupportedInEra EvieEra = Just UpdateProposalInEvieEra
updateProposalSupportedInEra JenEra    = Just UpdateProposalInJenEra
updateProposalSupportedInEra AurumEra  = Just UpdateProposalInAurumEra


-- ----------------------------------------------------------------------------
-- Building vs viewing transactions
--

data BuildTx
data ViewTx

data BuildTxWith build a where

     ViewTx      ::      BuildTxWith ViewTx  a
     BuildTxWith :: a -> BuildTxWith BuildTx a

deriving instance Eq   a => Eq   (BuildTxWith build a)
deriving instance Show a => Show (BuildTxWith build a)

-- ----------------------------------------------------------------------------
-- Transaction input values (era-dependent)
--

type TxIns build era = [(TxIn, BuildTxWith build (Witness WitCtxTxIn era))]

data TxInsCollateral era where

     TxInsCollateralNone :: TxInsCollateral era

     TxInsCollateral     :: CollateralSupportedInEra era
                         -> [TxIn] -- Only key witnesses, no scripts.
                         -> TxInsCollateral era

deriving instance Eq   (TxInsCollateral era)
deriving instance Show (TxInsCollateral era)


-- ----------------------------------------------------------------------------
-- Transaction output values (era-dependent)
--

data TxOutValue era where

     TxOutDafiOnly :: OnlyDafiSupportedInEra era -> Entropic -> TxOutValue era

     TxOutValue   :: MultiAssetSupportedInEra era -> Value -> TxOutValue era

deriving instance Eq   (TxOutValue era)
deriving instance Show (TxOutValue era)
deriving instance Generic (TxOutValue era)

instance ToJSON (TxOutValue era) where
  toJSON (TxOutDafiOnly _ ll) = toJSON ll
  toJSON (TxOutValue _ val) = toJSON val

instance IsBccEra era => FromJSON (TxOutValue era) where
  parseJSON = withObject "TxOutValue" $ \o -> do
    case multiAssetSupportedInEra bccEra of
      Left onlyDafi -> do
        ll <- o .: "entropic"
        pure $ TxOutDafiOnly onlyDafi $ selectEntropic ll
      Right maSupported -> do
        let l = HMS.toList o
        vals <- mapM decodeAssetId l
        pure $ TxOutValue maSupported $ mconcat vals
    where
     decodeAssetId :: (Text, Aeson.Value) -> Aeson.Parser Value
     decodeAssetId (polid, Aeson.Object assetNameHm) = do
       let polId = fromString $ Text.unpack polid
       aNameQuantity <- decodeAssets assetNameHm
       pure . valueFromList
         $ map (first $ AssetId polId) aNameQuantity

     decodeAssetId ("entropic", Aeson.Number sci) =
       case toBoundedInteger sci of
         Just (ll :: Word64) ->
           pure $ valueFromList [(DafiAssetId, Quantity $ toInteger ll)]
         Nothing ->
           fail $ "Expected a Bounded number but got: " <> show sci
     decodeAssetId wrong = fail $ "Expected a policy id and a JSON object but got: " <> show wrong

     decodeAssets :: Aeson.Object -> Aeson.Parser [(AssetName, Quantity)]
     decodeAssets assetNameHm =
       let l = HMS.toList assetNameHm
       in mapM (\(aName, q) -> (,) <$> parseAssetName aName <*> decodeQuantity q) l

     parseAssetName :: Text -> Aeson.Parser AssetName
     parseAssetName aName = runParsecParser assetName aName

     decodeQuantity :: Aeson.Value -> Aeson.Parser Quantity
     decodeQuantity (Aeson.Number sci) =
       case toBoundedInteger sci of
         Just (ll :: Word64) -> return . Quantity $ toInteger ll
         Nothing -> fail $ "Expected a Bounded number but got: " <> show sci
     decodeQuantity wrong = fail $ "Expected aeson Number but got: " <> show wrong

entropicToTxOutValue :: IsBccEra era => Entropic -> TxOutValue era
entropicToTxOutValue l =
    case multiAssetSupportedInEra bccEra of
      Left dafiOnly     -> TxOutDafiOnly dafiOnly  l
      Right multiAsset -> TxOutValue multiAsset (entropicToValue l)

txOutValueToEntropic :: TxOutValue era -> Entropic
txOutValueToEntropic tv =
  case tv of
    TxOutDafiOnly _ l -> l
    TxOutValue _ v -> selectEntropic v

txOutValueToValue :: TxOutValue era -> Value
txOutValueToValue tv =
  case tv of
    TxOutDafiOnly _ l -> entropicToValue l
    TxOutValue _ v -> v

prettyRenderTxOut :: TxOutInAnyEra -> Text
prettyRenderTxOut (TxOutInAnyEra _ (TxOut (AddressInEra _ addr) txOutVal _)) =
     serialiseAddress (toAddressAny addr) <> " + "
  <> renderValue (txOutValueToValue txOutVal)

-- ----------------------------------------------------------------------------
-- Transaction output datum (era-dependent)
--

data TxOutDatum ctx era where

     TxOutDatumNone :: TxOutDatum ctx era

     -- | A transaction output that only specifies the hash of the datum, but
     -- not the full datum value.
     --
     TxOutDatumHash :: ScriptDataSupportedInEra era
                    -> Hash ScriptData
                    -> TxOutDatum ctx era

     -- | A transaction output that specifies the whole datum value. This can
     -- only be used in the context of the transaction body, and does not occur
     -- in the UTxO. The UTxO only contains the datum hash.
     --
     TxOutDatum'    :: ScriptDataSupportedInEra era
                    -> Hash ScriptData
                    -> ScriptData
                    -> TxOutDatum CtxTx era

deriving instance Eq   (TxOutDatum ctx era)
deriving instance Show (TxOutDatum ctx era)

pattern TxOutDatum :: ScriptDataSupportedInEra era
                   -> ScriptData
                   -> TxOutDatum CtxTx era
pattern TxOutDatum s d  <- TxOutDatum' s _ d
  where
    TxOutDatum s d = TxOutDatum' s (hashScriptData d) d

{-# COMPLETE TxOutDatumNone, TxOutDatumHash, TxOutDatum' #-}
{-# COMPLETE TxOutDatumNone, TxOutDatumHash, TxOutDatum  #-}


parseHashScriptData :: Parsec.Parser (Hash ScriptData)
parseHashScriptData = do
  str <- Parsec.many1 Parsec.hexDigit Parsec.<?> "script data hash"
  case deserialiseFromRawBytesHex (AsHash AsScriptData) (BSC.pack str) of
    Just sdh -> return sdh
    Nothing  -> fail $ "Invalid datum hash: " ++ show str

-- ----------------------------------------------------------------------------
-- Transaction fees
--

data TxFee era where

     TxFeeImplicit :: TxFeesImplicitInEra era -> TxFee era

     TxFeeExplicit :: TxFeesExplicitInEra era -> Entropic -> TxFee era

deriving instance Eq   (TxFee era)
deriving instance Show (TxFee era)


-- ----------------------------------------------------------------------------
-- Transaction validity range
--

-- | This was formerly known as the TTL.
--
data TxValidityUpperBound era where

     TxValidityNoUpperBound :: ValidityNoUpperBoundSupportedInEra era
                            -> TxValidityUpperBound era

     TxValidityUpperBound   :: ValidityUpperBoundSupportedInEra era
                            -> SlotNo
                            -> TxValidityUpperBound era

deriving instance Eq   (TxValidityUpperBound era)
deriving instance Show (TxValidityUpperBound era)


data TxValidityLowerBound era where

     TxValidityNoLowerBound :: TxValidityLowerBound era

     TxValidityLowerBound   :: ValidityLowerBoundSupportedInEra era
                            -> SlotNo
                            -> TxValidityLowerBound era

deriving instance Eq   (TxValidityLowerBound era)
deriving instance Show (TxValidityLowerBound era)


-- ----------------------------------------------------------------------------
-- Transaction metadata (era-dependent)
--

data TxMetadataInEra era where

     TxMetadataNone  :: TxMetadataInEra era

     TxMetadataInEra :: TxMetadataSupportedInEra era
                     -> TxMetadata
                     -> TxMetadataInEra era

deriving instance Eq   (TxMetadataInEra era)
deriving instance Show (TxMetadataInEra era)


-- ----------------------------------------------------------------------------
-- Auxiliary scripts (era-dependent)
--

data TxAuxScripts era where

     TxAuxScriptsNone :: TxAuxScripts era

     TxAuxScripts     :: AuxScriptsSupportedInEra era
                      -> [ScriptInEra era]
                      -> TxAuxScripts era

deriving instance Eq   (TxAuxScripts era)
deriving instance Show (TxAuxScripts era)

-- ----------------------------------------------------------------------------
-- Optionally required signatures (era-dependent)
--

data TxExtraKeyWitnesses era where

  TxExtraKeyWitnessesNone :: TxExtraKeyWitnesses era

  TxExtraKeyWitnesses     :: TxExtraKeyWitnessesSupportedInEra era
                          -> [Hash PaymentKey]
                          -> TxExtraKeyWitnesses era

deriving instance Eq   (TxExtraKeyWitnesses era)
deriving instance Show (TxExtraKeyWitnesses era)

-- ----------------------------------------------------------------------------
-- Withdrawals within transactions (era-dependent)
--

data TxWithdrawals build era where

     TxWithdrawalsNone :: TxWithdrawals build era

     TxWithdrawals     :: WithdrawalsSupportedInEra era
                       -> [(StakeAddress, Entropic,
                            BuildTxWith build (Witness WitCtxStake era))]
                       -> TxWithdrawals build era

deriving instance Eq   (TxWithdrawals build era)
deriving instance Show (TxWithdrawals build era)


-- ----------------------------------------------------------------------------
-- Certificates within transactions (era-dependent)
--

data TxCertificates build era where

     TxCertificatesNone :: TxCertificates build era

     TxCertificates     :: CertificatesSupportedInEra era
                        -> [Certificate]
                        -> BuildTxWith build
                             (Map StakeCredential (Witness WitCtxStake era))
                        -> TxCertificates build era

deriving instance Eq   (TxCertificates build era)
deriving instance Show (TxCertificates build era)


-- ----------------------------------------------------------------------------
-- Transaction metadata (era-dependent)
--

data TxUpdateProposal era where

     TxUpdateProposalNone :: TxUpdateProposal era

     TxUpdateProposal     :: UpdateProposalSupportedInEra era
                          -> UpdateProposal
                          -> TxUpdateProposal era

deriving instance Eq   (TxUpdateProposal era)
deriving instance Show (TxUpdateProposal era)


-- ----------------------------------------------------------------------------
-- Value minting within transactions (era-dependent)
--

data TxMintValue build era where

     TxMintNone  :: TxMintValue build era

     TxMintValue :: MultiAssetSupportedInEra era
                 -> Value
                 -> BuildTxWith build
                      (Map PolicyId (ScriptWitness WitCtxMint era))
                 -> TxMintValue build era

deriving instance Eq   (TxMintValue build era)
deriving instance Show (TxMintValue build era)


-- ----------------------------------------------------------------------------
-- Transaction body content
--

data TxBodyContent build era =
     TxBodyContent {
       txIns            :: TxIns build era,
       txInsCollateral  :: TxInsCollateral era,
       txOuts           :: [TxOut CtxTx era],
       txFee            :: TxFee era,
       txValidityRange  :: (TxValidityLowerBound era,
                            TxValidityUpperBound era),
       txMetadata       :: TxMetadataInEra era,
       txAuxScripts     :: TxAuxScripts era,
       txExtraKeyWits   :: TxExtraKeyWitnesses era,
       txProtocolParams :: BuildTxWith build (Maybe ProtocolParameters),
       txWithdrawals    :: TxWithdrawals  build era,
       txCertificates   :: TxCertificates build era,
       txUpdateProposal :: TxUpdateProposal era,
       txMintValue      :: TxMintValue    build era,
       txScriptValidity :: TxScriptValidity era
     }


-- ----------------------------------------------------------------------------
-- Transaction bodies
--

data TxBody era where

     ColeTxBody
       :: Annotated Cole.Tx ByteString
       -> TxBody ColeEra

     SophieTxBody
       :: SophieBasedEra era
       -> Ledger.TxBody (SophieLedgerEra era)

          -- We include the scripts along with the tx body, rather than the
          -- witnesses set, since they need to be known when building the body.
       -> [Ledger.Script (SophieLedgerEra era)]

          -- The info for each use of each script: the script input data, both
          -- the UTxO input data (called the "datum") and the supplied input
          -- data (called the "redeemer") and the execution units.
       -> TxBodyScriptData era

          -- The 'Ledger.AuxiliaryData' consists of one or several things,
          -- depending on era:
          -- + transaction metadata  (in Sophie and later)
          -- + auxiliary scripts     (in Evie and later)
          -- Note that there is no auxiliary script data as such, because the
          -- extra script data has to be passed to scripts and hence is needed
          -- for validation. It is thus part of the witness data, not the
          -- auxiliary data.
       -> Maybe (Ledger.AuxiliaryData (SophieLedgerEra era))

       -> TxScriptValidity era -- ^ Mark script as expected to pass or fail validation

       -> TxBody era
     -- The 'SophieBasedEra' GADT tells us what era we are in.
     -- The 'SophieLedgerEra' type family maps that to the era type from the
     -- ledger lib. The 'Ledger.TxBody' type family maps that to a specific
     -- tx body type, which is different for each Sophie-based era.


data TxBodyScriptData era where
     TxBodyNoScriptData :: TxBodyScriptData era
     TxBodyScriptData   :: ScriptDataSupportedInEra era
                        -> Aurum.TxDats (SophieLedgerEra era)
                        -> Aurum.Redeemers (SophieLedgerEra era)
                        -> TxBodyScriptData era

deriving instance Eq   (TxBodyScriptData era)
deriving instance Show (TxBodyScriptData era)


-- The GADT in the SophieTxBody case requires a custom instance
instance Eq (TxBody era) where
    (==) (ColeTxBody txbodyA)
         (ColeTxBody txbodyB) = txbodyA == txbodyB

    (==) (SophieTxBody era txbodyA txscriptsA redeemersA txmetadataA scriptValidityA)
         (SophieTxBody _   txbodyB txscriptsB redeemersB txmetadataB scriptValidityB) =
         case era of
           SophieBasedEraSophie -> txbodyA     == txbodyB
                                  && txscriptsA  == txscriptsB
                                  && txmetadataA == txmetadataB

           SophieBasedEraEvie -> txbodyA     == txbodyB
                                  && txscriptsA  == txscriptsB
                                  && txmetadataA == txmetadataB

           SophieBasedEraJen    -> txbodyA     == txbodyB
                                  && txscriptsA  == txscriptsB
                                  && txmetadataA == txmetadataB

           SophieBasedEraAurum  -> txbodyA         == txbodyB
                                  && txscriptsA      == txscriptsB
                                  && redeemersA      == redeemersB
                                  && txmetadataA     == txmetadataB
                                  && scriptValidityA == scriptValidityB

    (==) ColeTxBody{} (SophieTxBody era _ _ _ _ _) = case era of {}


-- The GADT in the SophieTxBody case requires a custom instance
instance Show (TxBody era) where
    showsPrec p (ColeTxBody txbody) =
      showParen (p >= 11)
        ( showString "ColeTxBody "
        . showsPrec 11 txbody
        )

    showsPrec p (SophieTxBody SophieBasedEraSophie
                               txbody txscripts redeemers txmetadata scriptValidity) =
      showParen (p >= 11)
        ( showString "SophieTxBody SophieBasedEraSophie "
        . showsPrec 11 txbody
        . showChar ' '
        . showsPrec 11 txscripts
        . showChar ' '
        . showsPrec 11 redeemers
        . showChar ' '
        . showsPrec 11 txmetadata
        . showChar ' '
        . showsPrec 11 scriptValidity
        )

    showsPrec p (SophieTxBody SophieBasedEraEvie
                               txbody txscripts redeemers txmetadata scriptValidity) =
      showParen (p >= 11)
        ( showString "SophieTxBody SophieBasedEraEvie "
        . showsPrec 11 txbody
        . showChar ' '
        . showsPrec 11 txscripts
        . showChar ' '
        . showsPrec 11 redeemers
        . showChar ' '
        . showsPrec 11 txmetadata
        . showChar ' '
        . showsPrec 11 scriptValidity
        )

    showsPrec p (SophieTxBody SophieBasedEraJen
                               txbody txscripts redeemers txmetadata scriptValidity) =
      showParen (p >= 11)
        ( showString "SophieTxBody SophieBasedEraJen "
        . showsPrec 11 txbody
        . showChar ' '
        . showsPrec 11 txscripts
        . showChar ' '
        . showsPrec 11 redeemers
        . showChar ' '
        . showsPrec 11 txmetadata
        . showChar ' '
        . showsPrec 11 scriptValidity
        )

    showsPrec p (SophieTxBody SophieBasedEraAurum
                               txbody txscripts redeemers txmetadata scriptValidity) =
      showParen (p >= 11)
        ( showString "SophieTxBody SophieBasedEraJen "
        . showsPrec 11 txbody
        . showChar ' '
        . showsPrec 11 txscripts
        . showChar ' '
        . showsPrec 11 redeemers
        . showChar ' '
        . showsPrec 11 txmetadata
        . showChar ' '
        . showsPrec 11 scriptValidity
        )


instance HasTypeProxy era => HasTypeProxy (TxBody era) where
    data AsType (TxBody era) = AsTxBody (AsType era)
    proxyToAsType _ = AsTxBody (proxyToAsType (Proxy :: Proxy era))

pattern AsColeTxBody :: AsType (TxBody ColeEra)
pattern AsColeTxBody   = AsTxBody AsColeEra
{-# COMPLETE AsColeTxBody #-}

pattern AsSophieTxBody :: AsType (TxBody SophieEra)
pattern AsSophieTxBody = AsTxBody AsSophieEra
{-# COMPLETE AsSophieTxBody #-}

pattern AsJenTxBody :: AsType (TxBody JenEra)
pattern AsJenTxBody = AsTxBody AsJenEra
{-# COMPLETE AsJenTxBody #-}

instance IsBccEra era => SerialiseAsCBOR (TxBody era) where

    serialiseToCBOR (ColeTxBody txbody) =
      recoverBytes txbody

    serialiseToCBOR (SophieTxBody era txbody txscripts redeemers txmetadata scriptValidity) =
      case era of
        -- Use the same serialisation impl, but at different types:
        SophieBasedEraSophie -> serialiseSophieBasedTxBody
                                    era txbody txscripts redeemers txmetadata scriptValidity
        SophieBasedEraEvie -> serialiseSophieBasedTxBody
                                    era txbody txscripts redeemers txmetadata scriptValidity
        SophieBasedEraJen    -> serialiseSophieBasedTxBody
                                    era txbody txscripts redeemers txmetadata scriptValidity
        SophieBasedEraAurum  -> serialiseSophieBasedTxBody
                                    era txbody txscripts redeemers txmetadata scriptValidity

    deserialiseFromCBOR _ bs =
      case bccEra :: BccEra era of
        ColeEra ->
          ColeTxBody <$>
            CBOR.decodeFullAnnotatedBytes
              "Cole TxBody"
              CBOR.fromCBORAnnotated
              (LBS.fromStrict bs)

        -- Use the same derialisation impl, but at different types:
        SophieEra -> deserialiseSophieBasedTxBody SophieBasedEraSophie bs
        EvieEra -> deserialiseSophieBasedTxBody SophieBasedEraEvie bs
        JenEra    -> deserialiseSophieBasedTxBody SophieBasedEraJen    bs
        AurumEra  -> deserialiseSophieBasedTxBody SophieBasedEraAurum  bs

-- | The serialisation format for the different Sophie-based eras are not the
-- same, but they can be handled generally with one overloaded implementation.
serialiseSophieBasedTxBody
  :: forall era ledgerera.
     SophieLedgerEra era ~ ledgerera
  => ToCBOR (Ledger.TxBody ledgerera)
  => ToCBOR (Ledger.Script ledgerera)
  => ToCBOR (Aurum.TxDats ledgerera)
  => ToCBOR (Aurum.Redeemers ledgerera)
  => ToCBOR (Ledger.AuxiliaryData ledgerera)
  => SophieBasedEra era
  -> Ledger.TxBody ledgerera
  -> [Ledger.Script ledgerera]
  -> TxBodyScriptData era
  -> Maybe (Ledger.AuxiliaryData ledgerera)
  -> TxScriptValidity era -- ^ Mark script as expected to pass or fail validation
  -> ByteString
serialiseSophieBasedTxBody era txbody txscripts
                            TxBodyNoScriptData txmetadata scriptValidity =
    -- Backwards compat for pre-Aurum era tx body files
    case era of
      SophieBasedEraSophie -> preAurum
      SophieBasedEraEvie -> preAurum
      SophieBasedEraJen -> preAurum
      SophieBasedEraAurum ->
        CBOR.serializeEncoding'
          $ CBOR.encodeListLen 4
         <> CBOR.toCBOR txbody
         <> CBOR.toCBOR txscripts
         <> CBOR.toCBOR (txScriptValidityToScriptValidity scriptValidity)
         <> CBOR.encodeNullMaybe CBOR.toCBOR txmetadata
 where
   preAurum = CBOR.serializeEncoding'
                 $ CBOR.encodeListLen 3
                <> CBOR.toCBOR txbody
                <> CBOR.toCBOR txscripts
                <> CBOR.encodeNullMaybe CBOR.toCBOR txmetadata

serialiseSophieBasedTxBody _era txbody txscripts
                            (TxBodyScriptData _ datums redeemers)
                            txmetadata txBodycriptValidity =
    CBOR.serializeEncoding' $
        CBOR.encodeListLen 6
     <> CBOR.toCBOR txbody
     <> CBOR.toCBOR txscripts
     <> CBOR.toCBOR datums
     <> CBOR.toCBOR redeemers
     <> CBOR.toCBOR (txScriptValidityToScriptValidity txBodycriptValidity)
     <> CBOR.encodeNullMaybe CBOR.toCBOR txmetadata

deserialiseSophieBasedTxBody
  :: forall era ledgerera.
     SophieLedgerEra era ~ ledgerera
  => FromCBOR (CBOR.Annotator (Ledger.TxBody ledgerera))
  => FromCBOR (CBOR.Annotator (Ledger.Script ledgerera))
  => FromCBOR (CBOR.Annotator (Aurum.TxDats ledgerera))
  => FromCBOR (CBOR.Annotator (Aurum.Redeemers ledgerera))
  => FromCBOR (CBOR.Annotator (Ledger.AuxiliaryData ledgerera))
  => SophieBasedEra era
  -> ByteString
  -> Either CBOR.DecoderError (TxBody era)
deserialiseSophieBasedTxBody era bs =
    CBOR.decodeAnnotator
      "Sophie TxBody"
      decodeAnnotatedTuple
      (LBS.fromStrict bs)
  where
    decodeAnnotatedTuple :: CBOR.Decoder s (CBOR.Annotator (TxBody era))
    decodeAnnotatedTuple = do
      len <- CBOR.decodeListLen

      case len of
        -- Backwards compat for pre-Aurum era tx body files
        2 -> do
          txbody     <- fromCBOR
          txmetadata <- CBOR.decodeNullMaybe fromCBOR
          return $ CBOR.Annotator $ \fbs ->
            SophieTxBody era
              (flip CBOR.runAnnotator fbs txbody)
              [] -- scripts
              (flip CBOR.runAnnotator fbs (return TxBodyNoScriptData))
              (fmap (flip CBOR.runAnnotator fbs) txmetadata)
              (flip CBOR.runAnnotator fbs (return TxScriptValidityNone))
        3 -> do
          txbody     <- fromCBOR
          txscripts  <- fromCBOR
          txmetadata <- CBOR.decodeNullMaybe fromCBOR
          return $ CBOR.Annotator $ \fbs ->
            SophieTxBody era
              (flip CBOR.runAnnotator fbs txbody)
              (map (flip CBOR.runAnnotator fbs) txscripts)
              (flip CBOR.runAnnotator fbs (return TxBodyNoScriptData))
              (fmap (flip CBOR.runAnnotator fbs) txmetadata)
              (flip CBOR.runAnnotator fbs (return TxScriptValidityNone))
        4 -> do
          sValiditySupported <-
            case txScriptValiditySupportedInSophieBasedEra era of
              Nothing -> fail $ "deserialiseSophieBasedTxBody: Expected an era that supports the \
                                \script validity flag but got: "
                              <> show era
              Just supported -> return supported

          txbody     <- fromCBOR
          txscripts  <- fromCBOR
          scriptValidity <- fromCBOR
          txmetadata <- CBOR.decodeNullMaybe fromCBOR
          return $ CBOR.Annotator $ \fbs ->
            SophieTxBody era
              (flip CBOR.runAnnotator fbs txbody)
              (map (flip CBOR.runAnnotator fbs) txscripts)
              (flip CBOR.runAnnotator fbs (return TxBodyNoScriptData))
              (fmap (flip CBOR.runAnnotator fbs) txmetadata)
              (flip CBOR.runAnnotator fbs (return $ TxScriptValidity sValiditySupported scriptValidity))
        6 -> do
          sDataSupported <-
            case scriptDataSupportedInEra (sophieBasedToBccEra era) of
              Nothing -> fail $ "deserialiseSophieBasedTxBody: Expected an era that supports script\
                                \ data but got: "
                             <> show era
              Just supported -> return supported

          sValiditySupported <-
            case txScriptValiditySupportedInSophieBasedEra era of
              Nothing -> fail $ "deserialiseSophieBasedTxBody: Expected an era that supports the \
                                \script validity flag but got: "
                              <> show era
              Just supported -> return supported

          txbody    <- fromCBOR
          txscripts <- fromCBOR
          datums    <- fromCBOR
          redeemers <- fromCBOR
          scriptValidity <- fromCBOR
          txmetadata <- CBOR.decodeNullMaybe fromCBOR

          let txscriptdata = CBOR.Annotator $ \fbs ->
                               TxBodyScriptData sDataSupported
                                 (flip CBOR.runAnnotator fbs datums)
                                 (flip CBOR.runAnnotator fbs redeemers)

          return $ CBOR.Annotator $ \fbs ->
            SophieTxBody era
              (flip CBOR.runAnnotator fbs txbody)
              (map (flip CBOR.runAnnotator fbs) txscripts)
              (flip CBOR.runAnnotator fbs txscriptdata)
              (fmap (flip CBOR.runAnnotator fbs) txmetadata)
              (flip CBOR.runAnnotator fbs (return $ TxScriptValidity sValiditySupported scriptValidity))
        _ -> fail $ "expected tx body tuple of size 2, 3, 4 or 6, got " <> show len

instance IsBccEra era => HasTextEnvelope (TxBody era) where
    textEnvelopeType _ =
      case bccEra :: BccEra era of
        ColeEra   -> "TxUnsignedCole"
        SophieEra -> "TxUnsignedSophie"
        EvieEra -> "TxBodyEvie"
        JenEra    -> "TxBodyJen"
        AurumEra  -> "TxBodyAurum"


-- ----------------------------------------------------------------------------
-- Constructing transaction bodies
--

data TxBodyError =
       TxBodyEmptyTxIns
     | TxBodyEmptyTxInsCollateral
     | TxBodyEmptyTxOuts
     | TxBodyOutputNegative Quantity TxOutInAnyEra
     | TxBodyOutputOverflow Quantity TxOutInAnyEra
     | TxBodyMetadataError [(Word64, TxMetadataRangeError)]
     | TxBodyMintDafiError
     | TxBodyMissingProtocolParams
     deriving Show

instance Error TxBodyError where
    displayError TxBodyEmptyTxIns  = "Transaction body has no inputs"
    displayError TxBodyEmptyTxInsCollateral =
      "Transaction body has no collateral inputs, but uses Zerepoch scripts"
    displayError TxBodyEmptyTxOuts = "Transaction body has no outputs"
    displayError (TxBodyOutputNegative (Quantity q) txout) =
      "Negative quantity (" ++ show q ++ ") in transaction output: " ++
      show txout
    displayError (TxBodyOutputOverflow (Quantity q) txout) =
      "Quantity too large (" ++ show q ++ " >= 2^64) in transaction output: " ++
      show txout
    displayError (TxBodyMetadataError [(k, err)]) =
      "Error in metadata entry " ++ show k ++ ": " ++ displayError err
    displayError (TxBodyMetadataError errs) =
      "Error in metadata entries: " ++
      intercalate "; "
        [ show k ++ ": " ++ displayError err
        | (k, err) <- errs ]
    displayError TxBodyMintDafiError =
      "Transaction cannot mint dafi, only non-dafi assets"
    displayError TxBodyMissingProtocolParams =
      "Transaction uses Zerepoch scripts but does not provide the protocol " ++
      "parameters to hash"


makeTransactionBody :: forall era.
     IsBccEra era
  => TxBodyContent BuildTx era
  -> Either TxBodyError (TxBody era)
makeTransactionBody =
    case bccEraStyle (bccEra :: BccEra era) of
      LegacyColeEra      -> makeColeTransactionBody
      SophieBasedEra era -> makeSophieTransactionBody era


pattern TxBody :: TxBodyContent ViewTx era -> TxBody era
pattern TxBody txbodycontent <- (getTxBodyContent -> txbodycontent)
{-# COMPLETE TxBody #-}

getTxBodyContent :: TxBody era -> TxBodyContent ViewTx era
getTxBodyContent (ColeTxBody body) = getColeTxBodyContent body
getTxBodyContent (SophieTxBody era body _scripts scriptdata mAux scriptValidity) =
    fromLedgerTxBody era scriptValidity body scriptdata mAux

fromLedgerTxBody
  :: SophieBasedEra era
  -> TxScriptValidity era
  -> Ledger.TxBody (SophieLedgerEra era)
  -> TxBodyScriptData era
  -> Maybe (Ledger.AuxiliaryData (SophieLedgerEra era))
  -> TxBodyContent ViewTx era
fromLedgerTxBody era scriptValidity body scriptdata mAux =
    TxBodyContent
      { txIns            = fromLedgerTxIns            era body
      , txInsCollateral  = fromLedgerTxInsCollateral  era body
      , txOuts           = fromLedgerTxOuts           era body scriptdata
      , txFee            = fromLedgerTxFee            era body
      , txValidityRange  = fromLedgerTxValidityRange  era body
      , txWithdrawals    = fromLedgerTxWithdrawals    era body
      , txCertificates   = fromLedgerTxCertificates   era body
      , txUpdateProposal = fromLedgerTxUpdateProposal era body
      , txMintValue      = fromLedgerTxMintValue      era body
      , txExtraKeyWits   = fromLedgerTxExtraKeyWitnesses era body
      , txProtocolParams = ViewTx
      , txMetadata
      , txAuxScripts
      , txScriptValidity = scriptValidity
      }
  where
    (txMetadata, txAuxScripts) = fromLedgerTxAuxiliaryData era mAux


fromLedgerTxIns
  :: forall era.
     SophieBasedEra era
  -> Ledger.TxBody (SophieLedgerEra era)
  -> [(TxIn,BuildTxWith ViewTx (Witness WitCtxTxIn era))]
fromLedgerTxIns era body =
    [ (fromSophieTxIn input, ViewTx)
    | input <- Set.toList (inputs era body) ]
  where
    inputs :: SophieBasedEra era
           -> Ledger.TxBody (SophieLedgerEra era)
           -> Set (Ledger.TxIn StandardCrypto)
    inputs SophieBasedEraSophie = Sophie._inputs
    inputs SophieBasedEraEvie = Evie.inputs'
    inputs SophieBasedEraJen    = Jen.inputs'
    inputs SophieBasedEraAurum  = Aurum.inputs'


fromLedgerTxInsCollateral
  :: forall era.
     SophieBasedEra era
  -> Ledger.TxBody (SophieLedgerEra era)
  -> TxInsCollateral era
fromLedgerTxInsCollateral era body =
    case collateralSupportedInEra (sophieBasedToBccEra era) of
      Nothing        -> TxInsCollateralNone
      Just supported -> TxInsCollateral supported
                          [ fromSophieTxIn input
                          | input <- Set.toList (collateral era body) ]
  where
    collateral :: SophieBasedEra era
               -> Ledger.TxBody (SophieLedgerEra era)
               -> Set (Ledger.TxIn StandardCrypto)
    collateral SophieBasedEraSophie = const Set.empty
    collateral SophieBasedEraEvie = const Set.empty
    collateral SophieBasedEraJen    = const Set.empty
    collateral SophieBasedEraAurum  = Aurum.collateral'


fromLedgerTxOuts
  :: forall era.
     SophieBasedEra era
  -> Ledger.TxBody (SophieLedgerEra era)
  -> TxBodyScriptData era
  -> [TxOut CtxTx era]
fromLedgerTxOuts era body scriptdata =
  case era of
    SophieBasedEraSophie ->
      [ fromSophieTxOut era txout | txout <- toList (Sophie._outputs body) ]

    SophieBasedEraEvie ->
      [ fromSophieTxOut era txout | txout <- toList (Evie.outputs' body) ]

    SophieBasedEraJen ->
      [ fromSophieTxOut era txout | txout <- toList (Jen.outputs' body) ]

    SophieBasedEraAurum ->
      [ fromAurumTxOut
          MultiAssetInAurumEra
          ScriptDataInAurumEra
          txdatums
          txout
      | let txdatums = selectTxDatums scriptdata
      , txout <- toList (Aurum.outputs' body) ]
  where
    selectTxDatums  TxBodyNoScriptData                            = Map.empty
    selectTxDatums (TxBodyScriptData _ (Aurum.TxDats' datums) _) = datums

fromAurumTxOut :: forall era ledgerera.
                   IsSophieBasedEra era
                => Ledger.Era ledgerera
                => Ledger.Crypto ledgerera ~ StandardCrypto
                => Ledger.Value ledgerera ~ Jen.Value StandardCrypto
                => MultiAssetSupportedInEra era
                -> ScriptDataSupportedInEra era
                -> Map (Aurum.DataHash StandardCrypto)
                       (Aurum.Data ledgerera)
                -> Aurum.TxOut ledgerera
                -> TxOut CtxTx era
fromAurumTxOut multiAssetInEra scriptDataInEra txdatums
                (Aurum.TxOut addr value datahash) =
   TxOut (fromSophieAddr addr)
         (TxOutValue multiAssetInEra (fromJenValue value))
         (fromAurumTxOutDatum scriptDataInEra datahash)
  where
    fromAurumTxOutDatum :: ScriptDataSupportedInEra era
                         -> StrictMaybe (Aurum.DataHash StandardCrypto)
                         -> TxOutDatum CtxTx era
    fromAurumTxOutDatum _          SNothing = TxOutDatumNone
    fromAurumTxOutDatum supported (SJust dh)
      | Just d <- Map.lookup dh txdatums
                  = TxOutDatum'    supported (ScriptDataHash dh)
                                             (fromAurumData d)
      | otherwise = TxOutDatumHash supported (ScriptDataHash dh)

fromLedgerTxFee
  :: SophieBasedEra era -> Ledger.TxBody (SophieLedgerEra era) -> TxFee era
fromLedgerTxFee era body =
  case era of
    SophieBasedEraSophie ->
      TxFeeExplicit TxFeesExplicitInSophieEra $
      fromSophieEntropic $ Sophie._txfee body
    SophieBasedEraEvie ->
      TxFeeExplicit TxFeesExplicitInEvieEra $
      fromSophieEntropic $ Evie.txfee' body
    SophieBasedEraJen ->
      TxFeeExplicit TxFeesExplicitInJenEra $
      fromSophieEntropic $ Jen.txfee' body
    SophieBasedEraAurum ->
      TxFeeExplicit TxFeesExplicitInAurumEra $
      fromSophieEntropic $ Aurum.txfee' body

fromLedgerTxValidityRange
  :: SophieBasedEra era
  -> Ledger.TxBody (SophieLedgerEra era)
  -> (TxValidityLowerBound era, TxValidityUpperBound era)
fromLedgerTxValidityRange era body =
  case era of
    SophieBasedEraSophie ->
      ( TxValidityNoLowerBound
      , TxValidityUpperBound ValidityUpperBoundInSophieEra $ Sophie._ttl body
      )

    SophieBasedEraEvie ->
      ( case invalidBefore of
          SNothing -> TxValidityNoLowerBound
          SJust s  -> TxValidityLowerBound ValidityLowerBoundInEvieEra s
      , case invalidHereafter of
          SNothing -> TxValidityNoUpperBound ValidityNoUpperBoundInEvieEra
          SJust s  -> TxValidityUpperBound   ValidityUpperBoundInEvieEra s
      )
      where
        Evie.ValidityInterval{invalidBefore, invalidHereafter} =
          Evie.vldt' body

    SophieBasedEraJen ->
      ( case invalidBefore of
          SNothing -> TxValidityNoLowerBound
          SJust s  -> TxValidityLowerBound ValidityLowerBoundInJenEra s
      , case invalidHereafter of
          SNothing -> TxValidityNoUpperBound ValidityNoUpperBoundInJenEra
          SJust s  -> TxValidityUpperBound   ValidityUpperBoundInJenEra s
      )
      where
        Jen.ValidityInterval{invalidBefore, invalidHereafter} = Jen.vldt' body

    SophieBasedEraAurum ->
      ( case invalidBefore of
          SNothing -> TxValidityNoLowerBound
          SJust s  -> TxValidityLowerBound ValidityLowerBoundInAurumEra s
      , case invalidHereafter of
          SNothing -> TxValidityNoUpperBound ValidityNoUpperBoundInAurumEra
          SJust s  -> TxValidityUpperBound   ValidityUpperBoundInAurumEra s
      )
      where
        Jen.ValidityInterval{invalidBefore, invalidHereafter} = Aurum.vldt' body


fromLedgerAuxiliaryData
  :: SophieBasedEra era
  -> Ledger.AuxiliaryData (SophieLedgerEra era)
  -> (Map Word64 TxMetadataValue, [ScriptInEra era])
fromLedgerAuxiliaryData SophieBasedEraSophie (Sophie.Metadata metadata) =
  (fromSophieMetadata metadata, [])
fromLedgerAuxiliaryData SophieBasedEraEvie (Evie.AuxiliaryData ms ss) =
  ( fromSophieMetadata ms
  , fromSophieBasedScript SophieBasedEraEvie <$> toList ss
  )
fromLedgerAuxiliaryData SophieBasedEraJen (Jen.AuxiliaryData ms ss) =
  ( fromSophieMetadata ms
  , fromSophieBasedScript SophieBasedEraJen <$> toList ss
  )
fromLedgerAuxiliaryData SophieBasedEraAurum (Aurum.AuxiliaryData ms ss) =
  ( fromSophieMetadata ms
  , fromSophieBasedScript SophieBasedEraAurum <$> toList ss
  )

fromLedgerTxAuxiliaryData
  :: SophieBasedEra era
  -> Maybe (Ledger.AuxiliaryData (SophieLedgerEra era))
  -> (TxMetadataInEra era, TxAuxScripts era)
fromLedgerTxAuxiliaryData _ Nothing = (TxMetadataNone, TxAuxScriptsNone)
fromLedgerTxAuxiliaryData era (Just auxData) =
  case era of
    SophieBasedEraSophie ->
      ( if null ms then
          TxMetadataNone
        else
          TxMetadataInEra TxMetadataInSophieEra $ TxMetadata ms
      , TxAuxScriptsNone
      )
    SophieBasedEraEvie ->
      ( if null ms then
          TxMetadataNone
        else
          TxMetadataInEra TxMetadataInEvieEra $ TxMetadata ms
      , case ss of
          [] -> TxAuxScriptsNone
          _  -> TxAuxScripts AuxScriptsInEvieEra ss
      )
    SophieBasedEraJen ->
      ( if null ms then
          TxMetadataNone
        else
          TxMetadataInEra TxMetadataInJenEra $ TxMetadata ms
      , case ss of
          [] -> TxAuxScriptsNone
          _  -> TxAuxScripts AuxScriptsInJenEra ss
      )
    SophieBasedEraAurum ->
      ( if null ms then
          TxMetadataNone
        else
          TxMetadataInEra TxMetadataInAurumEra $ TxMetadata ms
      , case ss of
          [] -> TxAuxScriptsNone
          _  -> TxAuxScripts AuxScriptsInAurumEra ss
      )
  where
    (ms, ss) = fromLedgerAuxiliaryData era auxData


fromLedgerTxExtraKeyWitnesses :: SophieBasedEra era
                              -> Ledger.TxBody (SophieLedgerEra era)
                              -> TxExtraKeyWitnesses era
fromLedgerTxExtraKeyWitnesses sbe body =
  case sbe of
    SophieBasedEraSophie -> TxExtraKeyWitnessesNone
    SophieBasedEraEvie -> TxExtraKeyWitnessesNone
    SophieBasedEraJen    -> TxExtraKeyWitnessesNone
    SophieBasedEraAurum  -> TxExtraKeyWitnesses
                                ExtraKeyWitnessesInAurumEra
                                [ PaymentKeyHash (Sophie.coerceKeyRole keyhash)
                                | let keyhashes = Aurum.reqSignerHashes body
                                , keyhash <- Set.toList keyhashes ]

fromLedgerTxWithdrawals
  :: SophieBasedEra era
  -> Ledger.TxBody (SophieLedgerEra era)
  -> TxWithdrawals ViewTx era
fromLedgerTxWithdrawals era body =
  case era of
    SophieBasedEraSophie
      | null (Sophie.unWdrl withdrawals) -> TxWithdrawalsNone
      | otherwise ->
          TxWithdrawals WithdrawalsInSophieEra $
          fromSophieWithdrawal withdrawals
      where
        withdrawals = Sophie._wdrls body

    SophieBasedEraEvie
      | null (Sophie.unWdrl withdrawals) -> TxWithdrawalsNone
      | otherwise ->
          TxWithdrawals WithdrawalsInEvieEra $
          fromSophieWithdrawal withdrawals
      where
        withdrawals = Evie.wdrls' body

    SophieBasedEraJen
      | null (Sophie.unWdrl withdrawals) -> TxWithdrawalsNone
      | otherwise ->
          TxWithdrawals WithdrawalsInJenEra $ fromSophieWithdrawal withdrawals
      where
        withdrawals = Jen.wdrls' body

    SophieBasedEraAurum
      | null (Sophie.unWdrl withdrawals) -> TxWithdrawalsNone
      | otherwise ->
          TxWithdrawals WithdrawalsInAurumEra $ fromSophieWithdrawal withdrawals
      where
        withdrawals = Aurum.wdrls' body

fromLedgerTxCertificates
  :: SophieBasedEra era
  -> Ledger.TxBody (SophieLedgerEra era)
  -> TxCertificates ViewTx era
fromLedgerTxCertificates era body =
  case era of
    SophieBasedEraSophie
      | null certificates -> TxCertificatesNone
      | otherwise ->
          TxCertificates
            CertificatesInSophieEra
            (map fromSophieCertificate $ toList certificates)
            ViewTx
      where
        certificates = Sophie._certs body

    SophieBasedEraEvie
      | null certificates -> TxCertificatesNone
      | otherwise ->
          TxCertificates
            CertificatesInEvieEra
            (map fromSophieCertificate $ toList certificates)
            ViewTx
      where
        certificates = Evie.certs' body

    SophieBasedEraJen
      | null certificates -> TxCertificatesNone
      | otherwise ->
          TxCertificates
            CertificatesInJenEra
            (map fromSophieCertificate $ toList certificates)
            ViewTx
      where
        certificates = Jen.certs' body

    SophieBasedEraAurum
      | null certificates -> TxCertificatesNone
      | otherwise ->
          TxCertificates
            CertificatesInAurumEra
            (map fromSophieCertificate $ toList certificates)
            ViewTx
      where
        certificates = Aurum.certs' body

fromLedgerTxUpdateProposal
  :: SophieBasedEra era
  -> Ledger.TxBody (SophieLedgerEra era)
  -> TxUpdateProposal era
fromLedgerTxUpdateProposal era body =
  case era of
    SophieBasedEraSophie ->
      case Sophie._txUpdate body of
        SNothing -> TxUpdateProposalNone
        SJust p ->
          TxUpdateProposal UpdateProposalInSophieEra
                           (fromLedgerUpdate era p)

    SophieBasedEraEvie ->
      case Evie.update' body of
        SNothing -> TxUpdateProposalNone
        SJust p ->
          TxUpdateProposal UpdateProposalInEvieEra
                           (fromLedgerUpdate era p)

    SophieBasedEraJen ->
      case Jen.update' body of
        SNothing -> TxUpdateProposalNone
        SJust p ->
          TxUpdateProposal UpdateProposalInJenEra
                           (fromLedgerUpdate era p)

    SophieBasedEraAurum ->
      case Aurum.update' body of
        SNothing -> TxUpdateProposalNone
        SJust p ->
          TxUpdateProposal UpdateProposalInAurumEra
                           (fromLedgerUpdate era p)


fromLedgerTxMintValue
  :: SophieBasedEra era
  -> Ledger.TxBody (SophieLedgerEra era)
  -> TxMintValue ViewTx era
fromLedgerTxMintValue era body =
  case era of
    SophieBasedEraSophie -> TxMintNone
    SophieBasedEraEvie -> TxMintNone
    SophieBasedEraJen
      | isZero mint        -> TxMintNone
      | otherwise          -> TxMintValue MultiAssetInJenEra
                                          (fromJenValue mint) ViewTx
      where
        mint = Jen.mint' body

    SophieBasedEraAurum
      | isZero mint         -> TxMintNone
      | otherwise           -> TxMintValue MultiAssetInAurumEra
                                           (fromJenValue mint) ViewTx
      where
        mint = Aurum.mint' body


makeColeTransactionBody :: TxBodyContent BuildTx ColeEra
                         -> Either TxBodyError (TxBody ColeEra)
makeColeTransactionBody TxBodyContent { txIns, txOuts } = do
    ins'  <- NonEmpty.nonEmpty txIns      ?! TxBodyEmptyTxIns
    let ins'' = NonEmpty.map (toColeTxIn . fst) ins'

    outs'  <- NonEmpty.nonEmpty txOuts    ?! TxBodyEmptyTxOuts
    outs'' <- traverse
                (\out -> toColeTxOut out ?! classifyRangeError out)
                outs'
    return $
      ColeTxBody $
        reAnnotate $
          Annotated
            (Cole.UnsafeTx ins'' outs'' (Cole.mkAttributes ()))
            ()
  where
    classifyRangeError :: TxOut CtxTx ColeEra -> TxBodyError
    classifyRangeError
      txout@(TxOut (AddressInEra ColeAddressInAnyEra ColeAddress{})
                   (TxOutDafiOnly DafiOnlyInColeEra value) _)
      | value < 0        = TxBodyOutputNegative (entropicToQuantity value)
                                                (txOutInAnyEra txout)
      | otherwise        = TxBodyOutputOverflow (entropicToQuantity value)
                                                (txOutInAnyEra txout)

    classifyRangeError
      (TxOut (AddressInEra ColeAddressInAnyEra (ColeAddress _))
             (TxOutValue era _) _) = case era of {}

    classifyRangeError
      (TxOut (AddressInEra (SophieAddressInEra era) SophieAddress{})
             _ _) = case era of {}

getColeTxBodyContent :: Annotated Cole.Tx ByteString
                      -> TxBodyContent ViewTx ColeEra
getColeTxBodyContent (Annotated Cole.UnsafeTx{txInputs, txOutputs} _) =
    TxBodyContent {
      txIns            = [ (fromColeTxIn input, ViewTx)
                         | input <- toList txInputs],
      txInsCollateral  = TxInsCollateralNone,
      txOuts           = fromColeTxOut <$> toList txOutputs,
      txFee            = TxFeeImplicit TxFeesImplicitInColeEra,
      txValidityRange  = (TxValidityNoLowerBound,
                          TxValidityNoUpperBound
                            ValidityNoUpperBoundInColeEra),
      txMetadata       = TxMetadataNone,
      txAuxScripts     = TxAuxScriptsNone,
      txExtraKeyWits   = TxExtraKeyWitnessesNone,
      txProtocolParams = ViewTx,
      txWithdrawals    = TxWithdrawalsNone,
      txCertificates   = TxCertificatesNone,
      txUpdateProposal = TxUpdateProposalNone,
      txMintValue      = TxMintNone,
      txScriptValidity = TxScriptValidityNone
    }

makeSophieTransactionBody :: ()
  => SophieBasedEra era
  -> TxBodyContent BuildTx era
  -> Either TxBodyError (TxBody era)
makeSophieTransactionBody era@SophieBasedEraSophie
                           txbodycontent@TxBodyContent {
                             txIns,
                             txOuts,
                             txFee,
                             txValidityRange = (_, upperBound),
                             txMetadata,
                             txWithdrawals,
                             txCertificates,
                             txUpdateProposal
                           } = do

    guard (not (null txIns)) ?! TxBodyEmptyTxIns
    sequence_
      [ do guard (v >= 0) ?! TxBodyOutputNegative (entropicToQuantity v)
                                                  (txOutInAnyEra txout)
           guard (v <= maxTxOut) ?! TxBodyOutputOverflow (entropicToQuantity v)
                                                         (txOutInAnyEra txout)
      | let maxTxOut = fromIntegral (maxBound :: Word64) :: Entropic
      , txout@(TxOut _ (TxOutDafiOnly DafiOnlyInSophieEra v) _) <- txOuts ]
    case txMetadata of
      TxMetadataNone      -> return ()
      TxMetadataInEra _ m -> first TxBodyMetadataError (validateTxMetadata m)

    return $
      SophieTxBody era
        (Sophie.TxBody
          (Set.fromList (map (toSophieTxIn . fst) txIns))
          (Seq.fromList (map (toSophieTxOutAny era) txOuts))
          (case txCertificates of
             TxCertificatesNone    -> Seq.empty
             TxCertificates _ cs _ -> Seq.fromList (map toSophieCertificate cs))
          (case txWithdrawals of
             TxWithdrawalsNone  -> Sophie.Wdrl Map.empty
             TxWithdrawals _ ws -> toSophieWithdrawal ws)
          (case txFee of
             TxFeeImplicit era'  -> case era' of {}
             TxFeeExplicit _ fee -> toSophieEntropic fee)
          (case upperBound of
             TxValidityNoUpperBound era' -> case era' of {}
             TxValidityUpperBound _ ttl  -> ttl)
          (case txUpdateProposal of
             TxUpdateProposalNone -> SNothing
             TxUpdateProposal _ p -> SJust (toLedgerUpdate era p))
          (maybeToStrictMaybe
            (Ledger.hashAuxiliaryData <$> txAuxData)))
        scripts
        TxBodyNoScriptData
        txAuxData
        TxScriptValidityNone
  where
    scripts :: [Ledger.Script StandardSophie]
    scripts =
      [ toSophieScript (scriptWitnessScript scriptwitness)
      | (_, AnyScriptWitness scriptwitness)
          <- collectTxBodyScriptWitnesses txbodycontent
      ]

    txAuxData :: Maybe (Ledger.AuxiliaryData StandardSophie)
    txAuxData
      | Map.null ms = Nothing
      | otherwise   = Just (toSophieAuxiliaryData ms)
      where
        ms = case txMetadata of
               TxMetadataNone                     -> Map.empty
               TxMetadataInEra _ (TxMetadata ms') -> ms'

makeSophieTransactionBody era@SophieBasedEraEvie
                           txbodycontent@TxBodyContent {
                             txIns,
                             txOuts,
                             txFee,
                             txValidityRange = (lowerBound, upperBound),
                             txMetadata,
                             txAuxScripts,
                             txWithdrawals,
                             txCertificates,
                             txUpdateProposal
                           } = do

    guard (not (null txIns)) ?! TxBodyEmptyTxIns
    sequence_
      [ do guard (v >= 0) ?! TxBodyOutputNegative (entropicToQuantity v)
                                                  (txOutInAnyEra txout)
           guard (v <= maxTxOut) ?! TxBodyOutputOverflow (entropicToQuantity v)
                                                         (txOutInAnyEra txout)
      | let maxTxOut = fromIntegral (maxBound :: Word64) :: Entropic
      , txout@(TxOut _ (TxOutDafiOnly DafiOnlyInEvieEra v) _) <- txOuts
      ]
    case txMetadata of
      TxMetadataNone      -> return ()
      TxMetadataInEra _ m -> validateTxMetadata m ?!. TxBodyMetadataError

    return $
      SophieTxBody era
        (Evie.TxBody
          (Set.fromList (map (toSophieTxIn . fst) txIns))
          (Seq.fromList (map (toSophieTxOutAny era) txOuts))
          (case txCertificates of
             TxCertificatesNone    -> Seq.empty
             TxCertificates _ cs _ -> Seq.fromList (map toSophieCertificate cs))
          (case txWithdrawals of
             TxWithdrawalsNone  -> Sophie.Wdrl Map.empty
             TxWithdrawals _ ws -> toSophieWithdrawal ws)
          (case txFee of
             TxFeeImplicit era'  -> case era' of {}
             TxFeeExplicit _ fee -> toSophieEntropic fee)
          (Evie.ValidityInterval {
             invalidBefore    = case lowerBound of
                                          TxValidityNoLowerBound   -> SNothing
                                          TxValidityLowerBound _ s -> SJust s,
             invalidHereafter = case upperBound of
                                          TxValidityNoUpperBound _ -> SNothing
                                          TxValidityUpperBound _ s -> SJust s
           })
          (case txUpdateProposal of
             TxUpdateProposalNone -> SNothing
             TxUpdateProposal _ p -> SJust (toLedgerUpdate era p))
          (maybeToStrictMaybe
            (Ledger.hashAuxiliaryData <$> txAuxData))
          mempty) -- No minting in Evie, only Jen
        scripts
        TxBodyNoScriptData
        txAuxData
        TxScriptValidityNone
  where
    scripts :: [Ledger.Script StandardEvie]
    scripts =
      [ toSophieScript (scriptWitnessScript scriptwitness)
      | (_, AnyScriptWitness scriptwitness)
          <- collectTxBodyScriptWitnesses txbodycontent
      ]

    txAuxData :: Maybe (Ledger.AuxiliaryData StandardEvie)
    txAuxData
      | Map.null ms
      , null ss   = Nothing
      | otherwise = Just (toEvieAuxiliaryData ms ss)
      where
        ms = case txMetadata of
               TxMetadataNone                     -> Map.empty
               TxMetadataInEra _ (TxMetadata ms') -> ms'
        ss = case txAuxScripts of
               TxAuxScriptsNone   -> []
               TxAuxScripts _ ss' -> ss'

makeSophieTransactionBody era@SophieBasedEraJen
                           txbodycontent@TxBodyContent {
                             txIns,
                             txOuts,
                             txFee,
                             txValidityRange = (lowerBound, upperBound),
                             txMetadata,
                             txAuxScripts,
                             txWithdrawals,
                             txCertificates,
                             txUpdateProposal,
                             txMintValue
                           } = do

    guard (not (null txIns)) ?! TxBodyEmptyTxIns
    sequence_
      [ do allPositive
           allWithinMaxBound
      | let maxTxOut = fromIntegral (maxBound :: Word64) :: Quantity
      , txout@(TxOut _ (TxOutValue MultiAssetInJenEra v) _) <- txOuts
      , let allPositive =
              case [ q | (_,q) <- valueToList v, q < 0 ] of
                []  -> Right ()
                q:_ -> Left (TxBodyOutputNegative q (txOutInAnyEra txout))
            allWithinMaxBound =
              case [ q | (_,q) <- valueToList v, q > maxTxOut ] of
                []  -> Right ()
                q:_ -> Left (TxBodyOutputOverflow q (txOutInAnyEra txout))
      ]
    case txMetadata of
      TxMetadataNone      -> return ()
      TxMetadataInEra _ m -> validateTxMetadata m ?!. TxBodyMetadataError
    case txMintValue of
      TxMintNone        -> return ()
      TxMintValue _ v _ -> guard (selectEntropic v == 0) ?! TxBodyMintDafiError

    return $
      SophieTxBody era
        (Evie.TxBody
          (Set.fromList (map (toSophieTxIn . fst) txIns))
          (Seq.fromList (map (toSophieTxOutAny era) txOuts))
          (case txCertificates of
             TxCertificatesNone    -> Seq.empty
             TxCertificates _ cs _ -> Seq.fromList (map toSophieCertificate cs))
          (case txWithdrawals of
             TxWithdrawalsNone  -> Sophie.Wdrl Map.empty
             TxWithdrawals _ ws -> toSophieWithdrawal ws)
          (case txFee of
             TxFeeImplicit era'  -> case era' of {}
             TxFeeExplicit _ fee -> toSophieEntropic fee)
          (Evie.ValidityInterval {
             invalidBefore    = case lowerBound of
                                          TxValidityNoLowerBound   -> SNothing
                                          TxValidityLowerBound _ s -> SJust s,
             invalidHereafter = case upperBound of
                                          TxValidityNoUpperBound _ -> SNothing
                                          TxValidityUpperBound _ s -> SJust s
           })
          (case txUpdateProposal of
             TxUpdateProposalNone -> SNothing
             TxUpdateProposal _ p -> SJust (toLedgerUpdate era p))
          (maybeToStrictMaybe
            (Ledger.hashAuxiliaryData <$> txAuxData))
          (case txMintValue of
             TxMintNone        -> mempty
             TxMintValue _ v _ -> toJenValue v))
        scripts
        TxBodyNoScriptData
        txAuxData
        TxScriptValidityNone
  where
    scripts :: [Ledger.Script StandardJen]
    scripts =
      [ toSophieScript (scriptWitnessScript scriptwitness)
      | (_, AnyScriptWitness scriptwitness)
          <- collectTxBodyScriptWitnesses txbodycontent
      ]

    txAuxData :: Maybe (Ledger.AuxiliaryData StandardJen)
    txAuxData
      | Map.null ms
      , null ss   = Nothing
      | otherwise = Just (toEvieAuxiliaryData ms ss)
      where
        ms = case txMetadata of
               TxMetadataNone                     -> Map.empty
               TxMetadataInEra _ (TxMetadata ms') -> ms'
        ss = case txAuxScripts of
               TxAuxScriptsNone   -> []
               TxAuxScripts _ ss' -> ss'

makeSophieTransactionBody era@SophieBasedEraAurum
                           txbodycontent@TxBodyContent {
                             txIns,
                             txInsCollateral,
                             txOuts,
                             txFee,
                             txValidityRange = (lowerBound, upperBound),
                             txMetadata,
                             txAuxScripts,
                             txExtraKeyWits,
                             txProtocolParams,
                             txWithdrawals,
                             txCertificates,
                             txUpdateProposal,
                             txMintValue,
                             txScriptValidity
                           } = do

    guard (not (null txIns)) ?! TxBodyEmptyTxIns
    sequence_
      [ do allPositive
           allWithinMaxBound
      | let maxTxOut = fromIntegral (maxBound :: Word64) :: Quantity
      , txout@(TxOut _ (TxOutValue MultiAssetInAurumEra v) _) <- txOuts
      , let allPositive =
              case [ q | (_,q) <- valueToList v, q < 0 ] of
                []  -> Right ()
                q:_ -> Left (TxBodyOutputNegative q (txOutInAnyEra txout))
            allWithinMaxBound =
              case [ q | (_,q) <- valueToList v, q > maxTxOut ] of
                []  -> Right ()
                q:_ -> Left (TxBodyOutputOverflow q (txOutInAnyEra txout))
      ]
    case txMetadata of
      TxMetadataNone      -> return ()
      TxMetadataInEra _ m -> validateTxMetadata m ?!. TxBodyMetadataError
    case txMintValue of
      TxMintNone        -> return ()
      TxMintValue _ v _ -> guard (selectEntropic v == 0) ?! TxBodyMintDafiError
    case txInsCollateral of
      TxInsCollateralNone | not (Set.null languages)
        -> Left TxBodyEmptyTxInsCollateral
      _ -> return ()
    case txProtocolParams of
      BuildTxWith Nothing | not (Set.null languages)
        -> Left TxBodyMissingProtocolParams
      _ -> return () --TODO aurum: validate protocol params for the Aurum era.
                     --             All the necessary params must be provided.

    return $
      SophieTxBody era
        (Aurum.TxBody
          (Set.fromList (map (toSophieTxIn . fst) txIns))
          (case txInsCollateral of
             TxInsCollateralNone     -> Set.empty
             TxInsCollateral _ txins -> Set.fromList (map toSophieTxIn txins))
          (Seq.fromList (map (toSophieTxOutAny era) txOuts))
          (case txCertificates of
             TxCertificatesNone    -> Seq.empty
             TxCertificates _ cs _ -> Seq.fromList (map toSophieCertificate cs))
          (case txWithdrawals of
             TxWithdrawalsNone  -> Sophie.Wdrl Map.empty
             TxWithdrawals _ ws -> toSophieWithdrawal ws)
          (case txFee of
             TxFeeImplicit era'  -> case era' of {}
             TxFeeExplicit _ fee -> toSophieEntropic fee)
          (Evie.ValidityInterval {
             invalidBefore    = case lowerBound of
                                          TxValidityNoLowerBound   -> SNothing
                                          TxValidityLowerBound _ s -> SJust s,
             invalidHereafter = case upperBound of
                                          TxValidityNoUpperBound _ -> SNothing
                                          TxValidityUpperBound _ s -> SJust s
           })
          (case txUpdateProposal of
             TxUpdateProposalNone -> SNothing
             TxUpdateProposal _ p -> SJust (toLedgerUpdate era p))
          (case txExtraKeyWits of
             TxExtraKeyWitnessesNone   -> Set.empty
             TxExtraKeyWitnesses _ khs -> Set.fromList
                                            [ Sophie.coerceKeyRole kh
                                            | PaymentKeyHash kh <- khs ])
          (case txMintValue of
             TxMintNone        -> mempty
             TxMintValue _ v _ -> toJenValue v)
          (case txProtocolParams of
             BuildTxWith Nothing        -> SNothing
             BuildTxWith (Just pparams) ->
               Aurum.hashScriptIntegrity
                 (toLedgerPParams SophieBasedEraAurum pparams)
                 languages
                 redeemers
                 datums)
          (maybeToStrictMaybe
            (Ledger.hashAuxiliaryData <$> txAuxData))
          SNothing) -- TODO aurum: support optional network id in TxBodyContent
        scripts
        (TxBodyScriptData ScriptDataInAurumEra datums redeemers)
        txAuxData
        txScriptValidity
  where
    witnesses :: [(ScriptWitnessIndex, AnyScriptWitness AurumEra)]
    witnesses = collectTxBodyScriptWitnesses txbodycontent

    scripts :: [Ledger.Script StandardAurum]
    scripts =
      [ toSophieScript (scriptWitnessScript scriptwitness)
      | (_, AnyScriptWitness scriptwitness) <- witnesses
      ]

    datums :: Aurum.TxDats StandardAurum
    datums =
      Aurum.TxDats $
        Map.fromList
          [ (Aurum.hashData d', d')
          | d <- scriptdata
          , let d' = toAurumData d
          ]

    scriptdata :: [ScriptData]
    scriptdata =
        [ d | TxOut _ _ (TxOutDatum _ d) <- txOuts ]
     ++ [ d | (_, AnyScriptWitness
                    (ZerepochScriptWitness
                       _ _ _ (ScriptDatumForTxIn d) _ _)) <- witnesses
            ]

    redeemers :: Aurum.Redeemers StandardAurum
    redeemers =
      Aurum.Redeemers $
        Map.fromList
          [ (toAurumRdmrPtr idx, (toAurumData d, toAurumExUnits e))
          | (idx, AnyScriptWitness
                    (ZerepochScriptWitness _ _ _ _ d e)) <- witnesses
          ]

    languages :: Set Aurum.Language
    languages =
      Set.fromList
        [ toAurumLanguage (AnyZerepochScriptVersion v)
        | (_, AnyScriptWitness (ZerepochScriptWitness _ v _ _ _ _)) <- witnesses
        ]

    txAuxData :: Maybe (Ledger.AuxiliaryData StandardAurum)
    txAuxData
      | Map.null ms
      , null ss   = Nothing
      | otherwise = Just (toAurumAuxiliaryData ms ss)
      where
        ms = case txMetadata of
               TxMetadataNone                     -> Map.empty
               TxMetadataInEra _ (TxMetadata ms') -> ms'
        ss = case txAuxScripts of
               TxAuxScriptsNone   -> []
               TxAuxScripts _ ss' -> ss'

-- | A variant of 'toSophieTxOutAny that is used only internally to this module
-- that works with a 'TxOut' in any context (including CtxTx) by ignoring
-- embedded datums (taking only their hash).
--
toSophieTxOutAny :: forall ctx era ledgerera.
                   SophieLedgerEra era ~ ledgerera
                => SophieBasedEra era
                -> TxOut ctx era
                -> Ledger.TxOut ledgerera
toSophieTxOutAny era (TxOut _ (TxOutDafiOnly DafiOnlyInColeEra _) _) =
    case era of {}

toSophieTxOutAny _ (TxOut addr (TxOutDafiOnly DafiOnlyInSophieEra value) _) =
    Sophie.TxOut (toSophieAddr addr) (toSophieEntropic value)

toSophieTxOutAny _ (TxOut addr (TxOutDafiOnly DafiOnlyInEvieEra value) _) =
    Sophie.TxOut (toSophieAddr addr) (toSophieEntropic value)

toSophieTxOutAny _ (TxOut addr (TxOutValue MultiAssetInJenEra value) _) =
    Sophie.TxOut (toSophieAddr addr) (toJenValue value)

toSophieTxOutAny _ (TxOut addr (TxOutValue MultiAssetInAurumEra value) txoutdata) =
    Aurum.TxOut (toSophieAddr addr) (toJenValue value)
                 (toAurumTxOutDataHash' txoutdata)

toAurumTxOutDataHash' :: TxOutDatum ctx era
                      -> StrictMaybe (Aurum.DataHash StandardCrypto)
toAurumTxOutDataHash'  TxOutDatumNone                          = SNothing
toAurumTxOutDataHash' (TxOutDatumHash _ (ScriptDataHash dh))   = SJust dh
toAurumTxOutDataHash' (TxOutDatum'    _ (ScriptDataHash dh) _) = SJust dh


-- ----------------------------------------------------------------------------
-- Script witnesses within the tx body
--

-- | A 'ScriptWitness' in any 'WitCtx'. This lets us handle heterogeneous
-- collections of script witnesses from multiple contexts.
--
data AnyScriptWitness era where
     AnyScriptWitness :: ScriptWitness witctx era -> AnyScriptWitness era

-- | Identify the location of a 'ScriptWitness' within the context of a
-- 'TxBody'. These are indexes of the objects within the transaction that
-- need or can use script witnesses: inputs, minted assets, withdrawals and
-- certificates. These are simple numeric indices, enumerated from zero.
-- Thus the indices are not stable if the transaction body is modified.
--
data ScriptWitnessIndex =

     -- | The n'th transaction input, in the order of the 'TxId's.
     ScriptWitnessIndexTxIn !Word

     -- | The n'th minting 'PolicyId', in the order of the 'PolicyId's.
   | ScriptWitnessIndexMint !Word

     -- | The n'th certificate, in the list order of the certificates.
   | ScriptWitnessIndexCertificate !Word

     -- | The n'th withdrawal, in the order of the 'StakeAddress's.
   | ScriptWitnessIndexWithdrawal !Word
  deriving (Eq, Ord, Show)

renderScriptWitnessIndex :: ScriptWitnessIndex -> String
renderScriptWitnessIndex (ScriptWitnessIndexTxIn index) =
  "transaction input " <> show index <> " (in the order of the TxIds)"
renderScriptWitnessIndex (ScriptWitnessIndexMint index) =
  "policyId " <> show index <> " (in the order of the PolicyIds)"
renderScriptWitnessIndex (ScriptWitnessIndexCertificate index) =
  "certificate " <> show index <> " (in the list order of the certificates)"
renderScriptWitnessIndex (ScriptWitnessIndexWithdrawal index) =
  "withdrawal " <> show index <> " (in the order of the StakeAddresses)"

toAurumRdmrPtr :: ScriptWitnessIndex -> Aurum.RdmrPtr
toAurumRdmrPtr widx =
    case widx of
      ScriptWitnessIndexTxIn        n -> Aurum.RdmrPtr Aurum.Spend (fromIntegral n)
      ScriptWitnessIndexMint        n -> Aurum.RdmrPtr Aurum.Mint  (fromIntegral n)
      ScriptWitnessIndexCertificate n -> Aurum.RdmrPtr Aurum.Cert  (fromIntegral n)
      ScriptWitnessIndexWithdrawal  n -> Aurum.RdmrPtr Aurum.Rewrd (fromIntegral n)

fromAurumRdmrPtr :: Aurum.RdmrPtr -> ScriptWitnessIndex
fromAurumRdmrPtr (Aurum.RdmrPtr tag n) =
    case tag of
      Aurum.Spend -> ScriptWitnessIndexTxIn        (fromIntegral n)
      Aurum.Mint  -> ScriptWitnessIndexMint        (fromIntegral n)
      Aurum.Cert  -> ScriptWitnessIndexCertificate (fromIntegral n)
      Aurum.Rewrd -> ScriptWitnessIndexWithdrawal  (fromIntegral n)


mapTxScriptWitnesses :: forall era.
                        (forall witctx. ScriptWitnessIndex
                                     -> ScriptWitness witctx era
                                     -> ScriptWitness witctx era)
                     -> TxBodyContent BuildTx era
                     -> TxBodyContent BuildTx era
mapTxScriptWitnesses f txbodycontent@TxBodyContent {
                         txIns,
                         txWithdrawals,
                         txCertificates,
                         txMintValue
                       } =
    txbodycontent {
      txIns          = mapScriptWitnessesTxIns        txIns
    , txMintValue    = mapScriptWitnessesMinting      txMintValue
    , txCertificates = mapScriptWitnessesCertificates txCertificates
    , txWithdrawals  = mapScriptWitnessesWithdrawals  txWithdrawals
    }
  where
    mapScriptWitnessesTxIns
      :: [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))]
      -> [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))]
    mapScriptWitnessesTxIns txins =
        [ (txin, BuildTxWith wit')
          -- The tx ins are indexed in the map order by txid
        | (ix, (txin, BuildTxWith wit)) <- zip [0..] (orderTxIns txins)
        , let wit' = case wit of
                       KeyWitness{}              -> wit
                       ScriptWitness ctx witness -> ScriptWitness ctx witness'
                         where
                           witness' = f (ScriptWitnessIndexTxIn ix) witness
        ]

    mapScriptWitnessesWithdrawals
      :: TxWithdrawals BuildTx era
      -> TxWithdrawals BuildTx era
    mapScriptWitnessesWithdrawals  TxWithdrawalsNone = TxWithdrawalsNone
    mapScriptWitnessesWithdrawals (TxWithdrawals supported withdrawals) =
      TxWithdrawals supported
        [ (addr, withdrawal, BuildTxWith (ScriptWitness ctx witness'))
          -- The withdrawals are indexed in the map order by stake credential
        | (ix, (addr, withdrawal, BuildTxWith (ScriptWitness ctx witness)))
             <- zip [0..] (orderStakeAddrs withdrawals)
        , let witness' = f (ScriptWitnessIndexWithdrawal ix) witness
        ]

    mapScriptWitnessesCertificates
      :: TxCertificates BuildTx era
      -> TxCertificates BuildTx era
    mapScriptWitnessesCertificates  TxCertificatesNone = TxCertificatesNone
    mapScriptWitnessesCertificates (TxCertificates supported certs
                                                   (BuildTxWith witnesses)) =
      TxCertificates supported certs $ BuildTxWith $ Map.fromList
        [ (stakecred, ScriptWitness ctx witness')
          -- The certs are indexed in list order
        | (ix, cert) <- zip [0..] certs
        , stakecred  <- maybeToList (selectStakeCredential cert)
        , ScriptWitness ctx witness
                     <- maybeToList (Map.lookup stakecred witnesses)
        , let witness' = f (ScriptWitnessIndexCertificate ix) witness
        ]

    selectStakeCredential cert =
      case cert of
        StakeAddressDeregistrationCertificate stakecred   -> Just stakecred
        StakeAddressDelegationCertificate     stakecred _ -> Just stakecred
        _                                                 -> Nothing

    mapScriptWitnessesMinting
      :: TxMintValue BuildTx era
      -> TxMintValue BuildTx era
    mapScriptWitnessesMinting  TxMintNone = TxMintNone
    mapScriptWitnessesMinting (TxMintValue supported value
                                           (BuildTxWith witnesses)) =
      TxMintValue supported value $ BuildTxWith $ Map.fromList
        [ (policyid, witness')
          -- The minting policies are indexed in policy id order in the value
        | let ValueNestedRep bundle = valueToNestedRep value
        , (ix, ValueNestedBundle policyid _) <- zip [0..] bundle
        , witness <- maybeToList (Map.lookup policyid witnesses)
        , let witness' = f (ScriptWitnessIndexMint ix) witness
        ]


collectTxBodyScriptWitnesses :: forall era.
                                TxBodyContent BuildTx era
                             -> [(ScriptWitnessIndex, AnyScriptWitness era)]
collectTxBodyScriptWitnesses TxBodyContent {
                               txIns,
                               txWithdrawals,
                               txCertificates,
                               txMintValue
                             } =
    concat
      [ scriptWitnessesTxIns        txIns
      , scriptWitnessesWithdrawals  txWithdrawals
      , scriptWitnessesCertificates txCertificates
      , scriptWitnessesMinting      txMintValue
      ]
  where
    scriptWitnessesTxIns
      :: [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))]
      -> [(ScriptWitnessIndex, AnyScriptWitness era)]
    scriptWitnessesTxIns txins =
        [ (ScriptWitnessIndexTxIn ix, AnyScriptWitness witness)
          -- The tx ins are indexed in the map order by txid
        | (ix, (_, BuildTxWith (ScriptWitness _ witness)))
            <- zip [0..] (orderTxIns txins)
        ]

    scriptWitnessesWithdrawals
      :: TxWithdrawals BuildTx era
      -> [(ScriptWitnessIndex, AnyScriptWitness era)]
    scriptWitnessesWithdrawals  TxWithdrawalsNone = []
    scriptWitnessesWithdrawals (TxWithdrawals _ withdrawals) =
        [ (ScriptWitnessIndexWithdrawal ix, AnyScriptWitness witness)
          -- The withdrawals are indexed in the map order by stake credential
        | (ix, (_, _, BuildTxWith (ScriptWitness _ witness)))
             <- zip [0..] (orderStakeAddrs withdrawals)
        ]

    scriptWitnessesCertificates
      :: TxCertificates BuildTx era
      -> [(ScriptWitnessIndex, AnyScriptWitness era)]
    scriptWitnessesCertificates  TxCertificatesNone = []
    scriptWitnessesCertificates (TxCertificates _ certs (BuildTxWith witnesses)) =
        [ (ScriptWitnessIndexCertificate ix, AnyScriptWitness witness)
          -- The certs are indexed in list order
        | (ix, cert) <- zip [0..] certs
        , ScriptWitness _ witness <- maybeToList $ do
                                       stakecred <- selectStakeCredential cert
                                       Map.lookup stakecred witnesses
        ]

    selectStakeCredential cert =
      case cert of
        StakeAddressDeregistrationCertificate stakecred   -> Just stakecred
        StakeAddressDelegationCertificate     stakecred _ -> Just stakecred
        _                                                 -> Nothing

    scriptWitnessesMinting
      :: TxMintValue BuildTx era
      -> [(ScriptWitnessIndex, AnyScriptWitness era)]
    scriptWitnessesMinting  TxMintNone = []
    scriptWitnessesMinting (TxMintValue _ value (BuildTxWith witnesses)) =
        [ (ScriptWitnessIndexMint ix, AnyScriptWitness witness)
          -- The minting policies are indexed in policy id order in the value
        | let ValueNestedRep bundle = valueToNestedRep value
        , (ix, ValueNestedBundle policyid _) <- zip [0..] bundle
        , witness <- maybeToList (Map.lookup policyid witnesses)
        ]

-- This relies on the TxId Ord instance being consistent with the
-- Ledger.TxId Ord instance via the toSophieTxId conversion
-- This is checked by prop_ord_distributive_TxId
orderTxIns :: [(TxIn, v)] -> [(TxIn, v)]
orderTxIns = sortBy (compare `on` fst)

-- This relies on the StakeAddress Ord instance being consistent with the
-- Sophie.RewardAcnt Ord instance via the toSophieStakeAddr conversion
-- This is checked by prop_ord_distributive_StakeAddress
orderStakeAddrs :: [(StakeAddress, x, v)] -> [(StakeAddress, x, v)]
orderStakeAddrs = sortBy (compare `on` (\(k, _, _) -> k))


toSophieWithdrawal :: [(StakeAddress, Entropic, a)] -> Sophie.Wdrl StandardCrypto
toSophieWithdrawal withdrawals =
    Sophie.Wdrl $
      Map.fromList
        [ (toSophieStakeAddr stakeAddr, toSophieEntropic value)
        | (stakeAddr, value, _) <- withdrawals ]


fromSophieWithdrawal
  :: Sophie.Wdrl StandardCrypto
  -> [(StakeAddress, Entropic, BuildTxWith ViewTx (Witness WitCtxStake era))]
fromSophieWithdrawal (Sophie.Wdrl withdrawals) =
  [ (fromSophieStakeAddr stakeAddr, fromSophieEntropic value, ViewTx)
  | (stakeAddr, value) <- Map.assocs withdrawals
  ]


-- | In the Sophie era the auxiliary data consists only of the tx metadata
toSophieAuxiliaryData :: Map Word64 TxMetadataValue
                       -> Ledger.AuxiliaryData StandardSophie
toSophieAuxiliaryData m =
    Sophie.Metadata
      (toSophieMetadata m)


-- | In the Evie and Jen eras the auxiliary data consists of the tx metadata
-- and the axiliary scripts.
--
toEvieAuxiliaryData :: forall era ledgerera.
                          SophieLedgerEra era ~ ledgerera
                       => Ledger.AuxiliaryData ledgerera ~ Evie.AuxiliaryData ledgerera
                       => Ledger.AnnotatedData (Ledger.Script ledgerera)
                       => Ord (Ledger.Script ledgerera)
                       => Map Word64 TxMetadataValue
                       -> [ScriptInEra era]
                       -> Ledger.AuxiliaryData ledgerera
toEvieAuxiliaryData m ss =
    Evie.AuxiliaryData
      (toSophieMetadata m)
      (Seq.fromList (map toSophieScript ss))


-- | In the Aurum and later eras the auxiliary data consists of the tx metadata
-- and the axiliary scripts, and the axiliary script data.
--
toAurumAuxiliaryData :: forall era ledgerera.
                         SophieLedgerEra era ~ ledgerera
                      => Ledger.AuxiliaryData ledgerera ~ Aurum.AuxiliaryData ledgerera
                      => Ledger.Script ledgerera ~ Aurum.Script ledgerera
                      => Ledger.Era ledgerera
                      => Map Word64 TxMetadataValue
                      -> [ScriptInEra era]
                      -> Ledger.AuxiliaryData ledgerera
toAurumAuxiliaryData m ss =
    Aurum.AuxiliaryData
      (toSophieMetadata m)
      (Seq.fromList (map toSophieScript ss))


-- ----------------------------------------------------------------------------
-- Other utilities helpful with making transaction bodies
--

-- | Compute the 'TxIn' of the initial UTxO pseudo-transaction corresponding
-- to the given address in the genesis initial funds.
--
-- The Sophie initial UTxO is constructed from the 'sgInitialFunds' which
-- is not a full UTxO but just a map from addresses to coin values.
--
-- This gets turned into a UTxO by making a pseudo-transaction for each address,
-- with the 0th output being the coin value. So to spend from the initial UTxO
-- we need this same 'TxIn' to use as an input to the spending transaction.
--
genesisUTxOPseudoTxIn :: NetworkId -> Hash GenesisUTxOKey -> TxIn
genesisUTxOPseudoTxIn nw (GenesisUTxOKeyHash kh) =
    --TODO: should handle Cole UTxO case too.
    fromSophieTxIn (Sophie.initialFundsPseudoTxIn addr)
  where
    addr :: Sophie.Addr StandardCrypto
    addr = Sophie.Addr
             (toSophieNetwork nw)
             (Sophie.KeyHashObj kh)
             Sophie.StakeRefNull
