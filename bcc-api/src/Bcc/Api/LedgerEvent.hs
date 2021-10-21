{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Bcc.Api.LedgerEvent
  ( LedgerEvent (..),
    MIRDistributionDetails (..),
    PoolReapDetails (..),
    toLedgerEvent,
  )
where

import           Bcc.Api.Address (StakeCredential, fromSophieStakeCredential)
import           Bcc.Api.Block (EpochNo)
import           Bcc.Api.Certificate (Certificate)
import           Bcc.Api.KeysSophie (Hash (StakePoolKeyHash), StakePoolKey)
import           Bcc.Api.Value (Entropic, fromSophieDeltaEntropic, fromSophieEntropic)
import qualified Bcc.Ledger.Coin
import qualified Bcc.Ledger.Core as Ledger.Core
import qualified Bcc.Ledger.Credential
import           Bcc.Ledger.Crypto (StandardCrypto)
import           Bcc.Ledger.Era (Crypto)
import qualified Bcc.Ledger.Keys
import           Control.State.Transition (Event)
import           Data.Function (($), (.))
import           Data.Functor (fmap)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (Maybe (Just, Nothing))
import           Data.SOP.Strict
import           Shardagnostic.Consensus.Cole.Ledger.Block (ColeBlock)
import           Shardagnostic.Consensus.Bcc.Block (HardForkBlock)
import           Shardagnostic.Consensus.HardFork.Combinator.AcrossEras (getOneEraLedgerEvent)
import           Shardagnostic.Consensus.Ledger.Abstract (LedgerState)
import           Shardagnostic.Consensus.Ledger.Basics (AuxLedgerEvent)
import           Shardagnostic.Consensus.Sophie.Ledger (SophieBlock,
                   SophieLedgerEvent (SophieLedgerEventTICK))
import           Shardagnostic.Consensus.TypeFamilyWrappers
import           Bcc.Ledger.Sophie.API (InstantaneousRewards (InstantaneousRewards))
import           Bcc.Ledger.Sophie.Rules.Epoch (EpochEvent (PoolReapEvent))
import           Bcc.Ledger.Sophie.Rules.Mir (MirEvent (..))
import           Bcc.Ledger.Sophie.Rules.NewEpoch (NewEpochEvent (EpochEvent, MirEvent, SumRewards))
import           Bcc.Ledger.Sophie.Rules.PoolReap (PoolreapEvent (RetiredPools))
import           Bcc.Ledger.Sophie.Rules.Tick (TickEvent (NewEpochEvent))

data LedgerEvent
  = -- | The given pool is being registered for the first time on chain.
    PoolRegistration Certificate
  | -- | The given pool already exists and is being re-registered.
    PoolReRegistration Certificate
  | -- | Rewards are being distributed.
    RewardsDistribution EpochNo (Map StakeCredential Entropic)
  | -- | MIR are being distributed.
    MIRDistribution MIRDistributionDetails
  | -- | Pools have been reaped and deposits refunded.
    PoolReap PoolReapDetails

class ConvertLedgerEvent blk where
  toLedgerEvent :: WrapLedgerEvent blk -> Maybe LedgerEvent

instance ConvertLedgerEvent ColeBlock where
  toLedgerEvent _ = Nothing

instance
  ( Crypto ledgerera ~ StandardCrypto,
    Event (Ledger.Core.EraRule "TICK" ledgerera) ~ TickEvent ledgerera,
    Event (Ledger.Core.EraRule "NEWEPOCH" ledgerera) ~ NewEpochEvent ledgerera,
    Event (Ledger.Core.EraRule "EPOCH" ledgerera) ~ EpochEvent ledgerera,
    Event (Ledger.Core.EraRule "POOLREAP" ledgerera) ~ PoolreapEvent ledgerera,
    Event (Ledger.Core.EraRule "MIR" ledgerera) ~ MirEvent ledgerera
  ) =>
  ConvertLedgerEvent (SophieBlock ledgerera)
  where
  toLedgerEvent evt = case unwrapLedgerEvent evt of
    LESumRewards e m -> Just $ RewardsDistribution e m
    LEMirTransfer rp rt rtt ttr ->
      Just $
        MIRDistribution $
          MIRDistributionDetails rp rt rtt ttr
    LERetiredPools r u e -> Just $ PoolReap $ PoolReapDetails e r u
    _ -> Nothing

instance All ConvertLedgerEvent xs => ConvertLedgerEvent (HardForkBlock xs) where
  toLedgerEvent =
    hcollapse
      . hcmap (Proxy @ ConvertLedgerEvent) (K . toLedgerEvent)
      . getOneEraLedgerEvent
      . unwrapLedgerEvent

--------------------------------------------------------------------------------
-- Event details
--------------------------------------------------------------------------------

-- | Details of fund transfers due to MIR certificates.
--
--   Note that the transfers from reserves to treasury and treasury to reserves
--   are inverse; a transfer of 100 DAFI in either direction will result in a net
--   movement of 0, but we include both directions for assistance in debugging.
data MIRDistributionDetails = MIRDistributionDetails
  { reservePayouts :: Map StakeCredential Entropic,
    treasuryPayouts :: Map StakeCredential Entropic,
    reservesToTreasury :: Entropic,
    treasuryToReserves :: Entropic
  }

data PoolReapDetails = PoolReapDetails
  { epochNo :: EpochNo,
    -- | Refunded deposits. The pools referenced are now retired, and the
    --   'StakeCredential' accounts are credited with the deposits.
    refunded :: Map StakeCredential (Map (Hash StakePoolKey) Entropic),
    -- | Unclaimed deposits. The 'StakeCredential' referenced in this map is not
    -- actively registered at the time of the pool reaping, and as such the
    -- funds are returned to the treasury.
    unclaimed :: Map StakeCredential (Map (Hash StakePoolKey) Entropic)
  }

--------------------------------------------------------------------------------
-- Patterns for event access
--------------------------------------------------------------------------------

pattern LESumRewards ::
  ( Crypto ledgerera ~ StandardCrypto,
    Event (Ledger.Core.EraRule "TICK" ledgerera) ~ TickEvent ledgerera,
    Event (Ledger.Core.EraRule "NEWEPOCH" ledgerera) ~ NewEpochEvent ledgerera
  ) =>
  EpochNo ->
  Map StakeCredential Entropic ->
  AuxLedgerEvent (LedgerState (SophieBlock ledgerera))
pattern LESumRewards e m <-
  SophieLedgerEventTICK
    (NewEpochEvent (SumRewards e (convertSumRewardsMap -> m)))

pattern LEMirTransfer ::
  ( Crypto ledgerera ~ StandardCrypto,
    Event (Ledger.Core.EraRule "TICK" ledgerera) ~ TickEvent ledgerera,
    Event (Ledger.Core.EraRule "NEWEPOCH" ledgerera) ~ NewEpochEvent ledgerera,
    Event (Ledger.Core.EraRule "MIR" ledgerera) ~ MirEvent ledgerera
  ) =>
  Map StakeCredential Entropic ->
  Map StakeCredential Entropic ->
  Entropic ->
  Entropic ->
  AuxLedgerEvent (LedgerState (SophieBlock ledgerera))
pattern LEMirTransfer rp tp rtt ttr <-
  SophieLedgerEventTICK
    ( NewEpochEvent
        ( MirEvent
            ( MirTransfer
                ( InstantaneousRewards
                    (convertSumRewardsMap -> rp)
                    (convertSumRewardsMap -> tp)
                    (fromSophieDeltaEntropic -> rtt)
                    (fromSophieDeltaEntropic -> ttr)
                  )
              )
          )
      )

convertSumRewardsMap ::
  Map
    ( Bcc.Ledger.Credential.StakeCredential
        Bcc.Ledger.Crypto.StandardCrypto
    )
    Bcc.Ledger.Coin.Coin ->
  Map StakeCredential Entropic
convertSumRewardsMap =
  Map.mapKeys fromSophieStakeCredential . fmap fromSophieEntropic

pattern LERetiredPools ::
  ( Crypto ledgerera ~ StandardCrypto,
    Event (Ledger.Core.EraRule "TICK" ledgerera) ~ TickEvent ledgerera,
    Event (Ledger.Core.EraRule "NEWEPOCH" ledgerera) ~ NewEpochEvent ledgerera,
    Event (Ledger.Core.EraRule "EPOCH" ledgerera) ~ EpochEvent ledgerera,
    Event (Ledger.Core.EraRule "POOLREAP" ledgerera) ~ PoolreapEvent ledgerera
  ) =>
  Map StakeCredential (Map (Hash StakePoolKey) Entropic) ->
  Map StakeCredential (Map (Hash StakePoolKey) Entropic) ->
  EpochNo ->
  AuxLedgerEvent (LedgerState (SophieBlock ledgerera))
pattern LERetiredPools r u e <-
  SophieLedgerEventTICK
    ( NewEpochEvent
        ( EpochEvent
            ( PoolReapEvent
                ( RetiredPools
                    (convertRetiredPoolsMap -> r)
                    (convertRetiredPoolsMap -> u)
                    e
                  )
              )
          )
      )

convertRetiredPoolsMap ::
  Map
    ( Bcc.Ledger.Credential.StakeCredential
        Bcc.Ledger.Crypto.StandardCrypto
    )
    ( Map
        (Bcc.Ledger.Keys.KeyHash Bcc.Ledger.Keys.StakePool StandardCrypto)
        Bcc.Ledger.Coin.Coin
    ) ->
  Map StakeCredential (Map (Hash StakePoolKey) Entropic)
convertRetiredPoolsMap =
  Map.mapKeys fromSophieStakeCredential
    . fmap (Map.mapKeys StakePoolKeyHash . fmap fromSophieEntropic)
