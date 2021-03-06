{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Bcc.Tracing.OrphanInstances.Cole () where

import           Bcc.Prelude

import           Data.Aeson (Value (..))
import qualified Data.Set as Set
import qualified Data.Text as Text

import           Bcc.Tracing.OrphanInstances.Common
import           Bcc.Tracing.OrphanInstances.Consensus ()

import           Shardagnostic.Consensus.Block (Header)
import           Shardagnostic.Network.Block (blockHash, blockNo, blockSlot)

import           Shardagnostic.Consensus.Cole.Ledger (ColeBlock (..),
                   ColeOtherHeaderEnvelopeError (..), TxId (..), coleHeaderRaw)
import           Shardagnostic.Consensus.Cole.Ledger.Inspect (ColeLedgerUpdate (..),
                   ProtocolUpdate (..), UpdateState (..))
import           Shardagnostic.Consensus.Ledger.SupportsMempool (GenTx, txId)
import           Shardagnostic.Consensus.Util.Condense (condense)

import           Bcc.Chain.Block (ABlockOrBoundaryHdr (..), AHeader (..),
                   ChainValidationError (..), delegationCertificate)
import           Bcc.Chain.Cole.API (ApplyMempoolPayloadErr (..))
import           Bcc.Chain.Delegation (delegateVK)
import           Bcc.Crypto.Signing (VerificationKey)

{- HLINT ignore "Use :" -}

--
-- | instances of @ToObject@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance ToObject ApplyMempoolPayloadErr where
  toObject _verb (MempoolTxErr utxoValidationErr) =
    mkObject
      [ "kind" .= String "MempoolTxErr"
      , "error" .= String (show utxoValidationErr)
      ]
  toObject _verb (MempoolDlgErr delegScheduleError) =
    mkObject
      [ "kind" .= String "MempoolDlgErr"
      , "error" .= String (show delegScheduleError)
      ]
  toObject _verb (MempoolUpdateProposalErr iFaceErr) =
    mkObject
      [ "kind" .= String "MempoolUpdateProposalErr"
      , "error" .= String (show iFaceErr)
      ]
  toObject _verb (MempoolUpdateVoteErr iFaceErrr) =
    mkObject
      [ "kind" .= String "MempoolUpdateVoteErr"
      , "error" .= String (show iFaceErrr)
      ]

instance ToObject ColeLedgerUpdate where
  toObject verb (ColeUpdatedProtocolUpdates protocolUpdates) =
    mkObject
      [ "kind"            .= String "ColeUpdatedProtocolUpdates"
      , "protocolUpdates" .= map (toObject verb) protocolUpdates
      ]

instance ToObject ProtocolUpdate where
  toObject verb (ProtocolUpdate updateVersion updateState) =
    mkObject
      [ "kind"                  .= String "ProtocolUpdate"
      , "protocolUpdateVersion" .= updateVersion
      , "protocolUpdateState"   .= toObject verb updateState
      ]

instance ToObject UpdateState where
  toObject _verb updateState = case updateState of
      UpdateRegistered slot ->
        mkObject
          [ "kind" .= String "UpdateRegistered"
          , "slot" .= slot
          ]
      UpdateActive votes ->
        mkObject
          [ "kind"  .= String "UpdateActive"
          , "votes" .= map (Text.pack . show) (Set.toList votes)
          ]
      UpdateConfirmed slot ->
        mkObject
          [ "kind" .= String "UpdateConfirmed"
          , "slot" .= slot
          ]
      UpdateStablyConfirmed endorsements ->
        mkObject
          [ "kind"         .= String "UpdateStablyConfirmed"
          , "endorsements" .= map (Text.pack . show) (Set.toList endorsements)
          ]
      UpdateCandidate slot epoch ->
        mkObject
          [ "kind" .= String "UpdateCandidate"
          , "slot" .= slot
          , "epoch" .= epoch
          ]
      UpdateStableCandidate transitionEpoch ->
        mkObject
          [ "kind"            .= String "UpdateStableCandidate"
          , "transitionEpoch" .= transitionEpoch
          ]

instance ToObject (GenTx ColeBlock) where
  toObject verb tx =
    mkObject $
        [ "txid" .= txId tx ]
     ++ [ "tx"   .= condense tx | verb == MaximalVerbosity ]


instance ToJSON (TxId (GenTx ColeBlock)) where
  toJSON (ColeTxId             i) = toJSON (condense i)
  toJSON (ColeDlgId            i) = toJSON (condense i)
  toJSON (ColeUpdateProposalId i) = toJSON (condense i)
  toJSON (ColeUpdateVoteId     i) = toJSON (condense i)


instance ToObject ChainValidationError where
  toObject _verb ChainValidationBoundaryTooLarge =
    mkObject
      [ "kind" .= String "ChainValidationBoundaryTooLarge" ]
  toObject _verb ChainValidationBlockAttributesTooLarge =
    mkObject
      [ "kind" .= String "ChainValidationBlockAttributesTooLarge" ]
  toObject _verb (ChainValidationBlockTooLarge _ _) =
    mkObject
      [ "kind" .= String "ChainValidationBlockTooLarge" ]
  toObject _verb ChainValidationHeaderAttributesTooLarge =
    mkObject
      [ "kind" .= String "ChainValidationHeaderAttributesTooLarge" ]
  toObject _verb (ChainValidationHeaderTooLarge _ _) =
    mkObject
      [ "kind" .= String "ChainValidationHeaderTooLarge" ]
  toObject _verb (ChainValidationDelegationPayloadError err) =
    mkObject
      [ "kind" .= String err ]
  toObject _verb (ChainValidationInvalidDelegation _ _) =
    mkObject
      [ "kind" .= String "ChainValidationInvalidDelegation" ]
  toObject _verb (ChainValidationGenesisHashMismatch _ _) =
    mkObject
      [ "kind" .= String "ChainValidationGenesisHashMismatch" ]
  toObject _verb (ChainValidationExpectedGenesisHash _ _) =
    mkObject
      [ "kind" .= String "ChainValidationExpectedGenesisHash" ]
  toObject _verb (ChainValidationExpectedHeaderHash _ _) =
    mkObject
      [ "kind" .= String "ChainValidationExpectedHeaderHash" ]
  toObject _verb (ChainValidationInvalidHash _ _) =
    mkObject
      [ "kind" .= String "ChainValidationInvalidHash" ]
  toObject _verb (ChainValidationMissingHash _) =
    mkObject
      [ "kind" .= String "ChainValidationMissingHash" ]
  toObject _verb (ChainValidationUnexpectedGenesisHash _) =
    mkObject
      [ "kind" .= String "ChainValidationUnexpectedGenesisHash" ]
  toObject _verb (ChainValidationInvalidSignature _) =
    mkObject
      [ "kind" .= String "ChainValidationInvalidSignature" ]
  toObject _verb (ChainValidationDelegationSchedulingError _) =
    mkObject
      [ "kind" .= String "ChainValidationDelegationSchedulingError" ]
  toObject _verb (ChainValidationProtocolMagicMismatch _ _) =
    mkObject
      [ "kind" .= String "ChainValidationProtocolMagicMismatch" ]
  toObject _verb ChainValidationSignatureLight =
    mkObject
      [ "kind" .= String "ChainValidationSignatureLight" ]
  toObject _verb (ChainValidationTooManyDelegations _) =
    mkObject
      [ "kind" .= String "ChainValidationTooManyDelegations" ]
  toObject _verb (ChainValidationUpdateError _ _) =
    mkObject
      [ "kind" .= String "ChainValidationUpdateError" ]
  toObject _verb (ChainValidationUTxOValidationError _) =
    mkObject
      [ "kind" .= String "ChainValidationUTxOValidationError" ]
  toObject _verb (ChainValidationProofValidationError _) =
    mkObject
      [ "kind" .= String "ChainValidationProofValidationError" ]


instance ToObject (Header ColeBlock) where
  toObject _verb b =
    mkObject $
        [ "kind" .= String "ColeBlock"
        , "hash" .= condense (blockHash b)
        , "slotNo" .= condense (blockSlot b)
        , "blockNo" .= condense (blockNo b)
        ] <>
        case coleHeaderRaw b of
          ABOBBoundaryHdr{} -> []
          ABOBBlockHdr h ->
            [ "delegate" .= condense (headerSignerVk h) ]
   where
     headerSignerVk :: AHeader ByteString -> VerificationKey
     headerSignerVk =
       delegateVK . delegationCertificate . headerSignature


instance ToObject ColeOtherHeaderEnvelopeError where
  toObject _verb (UnexpectedEBBInSlot slot) =
    mkObject
      [ "kind" .= String "UnexpectedEBBInSlot"
      , "slot" .= slot
      ]
