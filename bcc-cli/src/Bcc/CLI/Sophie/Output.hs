{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Bcc.CLI.Sophie.Output
  ( QueryTipLocalState(..)
  , QueryTipLocalStateOutput(..)
  ) where

import           Bcc.Api

import           Bcc.CLI.Sophie.Orphans ()
import           Bcc.Prelude (Text)
import           Bcc.Slotting.Time (SystemStart (..))
import           Data.Aeson (KeyValue, ToJSON (..), (.=))
import           Data.Function (id, ($), (.))
import           Data.Maybe ( Maybe(..) )
import           Data.Monoid (mconcat)
import           Bcc.Ledger.Sophie.Scripts ()

import qualified Data.Aeson as J

data QueryTipLocalState mode = QueryTipLocalState
  { era :: AnyBccEra
  , eraHistory :: EraHistory BccMode
  , mSystemStart :: Maybe SystemStart
  , mChainTip :: Maybe ChainTip
  }

data QueryTipLocalStateOutput = QueryTipLocalStateOutput
  { localStateChainTip :: ChainTip
  , mEra :: Maybe AnyBccEra
  , mEpoch :: Maybe EpochNo
  , mSyncProgress :: Maybe Text
  }

-- | A key-value pair difference list for encoding a JSON object.
(..=) :: (KeyValue kv, ToJSON v) => Text -> v -> [kv] -> [kv]
(..=) n v = (n .= v:)

-- | A key-value pair difference list for encoding a JSON object where Nothing encodes absence of the key-value pair.
(..=?) :: (KeyValue kv, ToJSON v) => Text -> Maybe v -> [kv] -> [kv]
(..=?) n mv = case mv of
  Just v -> (n .= v:)
  Nothing -> id

instance ToJSON QueryTipLocalStateOutput where
  toJSON a = case localStateChainTip a of
    ChainTipAtGenesis ->
      J.object $
        ( ("era" ..=? mEra a)
        . ("epoch" ..=? mEpoch a)
        . ("syncProgress" ..=? mSyncProgress a)
        ) []
    ChainTip slotNo blockHeader blockNo ->
      J.object $
        ( ("slot" ..= slotNo)
        . ("hash" ..= serialiseToRawBytesHexText blockHeader)
        . ("block" ..= blockNo)
        . ("era" ..=? mEra a)
        . ("epoch" ..=? mEpoch a)
        . ("syncProgress" ..=? mSyncProgress a)
        ) []
  toEncoding a = case localStateChainTip a of
    ChainTipAtGenesis ->
      J.pairs $ mconcat $
        ( ("era" ..=? mEra a)
        . ("epoch" ..=? mEpoch a)
        . ("syncProgress" ..=? mSyncProgress a)
        ) []
    ChainTip slotNo blockHeader blockNo ->
      J.pairs $ mconcat $
        ( ("slot" ..= slotNo)
        . ("hash" ..= serialiseToRawBytesHexText blockHeader)
        . ("block" ..= blockNo)
        . ("era" ..=? mEra a)
        . ("epoch" ..=? mEpoch a)
        . ("syncProgress" ..=? mSyncProgress a)
        ) []
