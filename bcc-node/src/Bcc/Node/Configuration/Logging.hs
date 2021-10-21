{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bcc.Node.Configuration.Logging
  ( LoggingLayer (..)
  , EKGDirect(..)
  , createLoggingLayer
  , nodeBasicInfo
  , shutdownLoggingLayer
  , traceCounter
  -- re-exports
  , Trace
  , Configuration
  , LoggerName
  , Severity (..)
  , mkLOMeta
  , LOMeta (..)
  , LOContent (..)
  ) where

import           Bcc.Prelude hiding (trace)

import qualified Control.Concurrent.Async as Async
import           Control.Exception.Safe (MonadCatch)
import           Control.Monad.Trans.Except.Extra (catchIOExceptT)
import           Control.Tracer
import           Data.List (nub)
import qualified Data.Map as Map
import           Data.Text (pack)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Version (showVersion)
import           System.Metrics.Counter (Counter)
import           System.Metrics.Gauge (Gauge)
import           System.Metrics.Label (Label)
import qualified System.Remote.Monitoring as EKG

import           Bcc.BM.Backend.Aggregation (plugin)
import           Bcc.BM.Backend.EKGView (plugin)
import           Bcc.BM.Backend.Monitoring (plugin)
import           Bcc.BM.Backend.Switchboard (Switchboard)
import qualified Bcc.BM.Backend.Switchboard as Switchboard
import           Bcc.BM.Backend.TraceForwarder (plugin)
import           Bcc.BM.Configuration (Configuration)
import qualified Bcc.BM.Configuration as Config
import qualified Bcc.BM.Configuration.Model as Config
import           Bcc.BM.Data.Aggregated (Measurable (..))
import           Bcc.BM.Data.Backend (Backend, BackendKind (..))
import           Bcc.BM.Data.LogItem (LOContent (..), LOMeta (..), LoggerName)
import qualified Bcc.BM.Observer.Monadic as Monadic
import qualified Bcc.BM.Observer.STM as Stm
import           Bcc.BM.Plugin (loadPlugin)
#if defined(SYSTEMD)
import           Bcc.BM.Scribe.Systemd (plugin)
#endif
import           Bcc.BM.Setup (setupTrace_, shutdown)
import           Bcc.BM.Stats
import           Bcc.BM.Stats.Resources
import qualified Bcc.BM.Trace as Trace
import           Bcc.BM.Tracing

import qualified Bcc.Chain.Genesis as Gen
import           Bcc.Slotting.Slot (EpochSize (..))
import qualified Shardagnostic.Consensus.BlockchainTime.WallClock.Types as WCT
import           Shardagnostic.Consensus.Cole.Ledger.Conversions
import           Shardagnostic.Consensus.Bcc.Block
import           Shardagnostic.Consensus.Bcc.CanHardFork
import qualified Shardagnostic.Consensus.Config as Consensus
import           Shardagnostic.Consensus.Config.SupportsNode (ConfigSupportsNode (..))
import           Shardagnostic.Consensus.HardFork.Combinator.Degenerate
import           Shardagnostic.Consensus.Node.ProtocolInfo
import           Shardagnostic.Consensus.Sophie.Ledger.Ledger
import qualified Bcc.Ledger.Sophie.API as SL

import           Bcc.Api.Protocol.Types (BlockType (..), protocolInfo)
import           Bcc.Config.Git.Rev (gitRev)
import           Bcc.Node.Configuration.POM (NodeConfiguration (..), ncProtocol)
import           Bcc.Node.Protocol.Types (SomeConsensusProtocol (..))
import           Bcc.Node.Types
import           Bcc.Tracing.OrphanInstances.Common ()
import           Paths_bcc_node (version)

--------------------------------
-- Layer
--------------------------------

-- | The LoggingLayer interface that we can expose.
-- We want to do this since we want to be able to mock out any function tied to logging.
--
-- The good side of this is that _each function has it's own effects_
-- and that is ideal for tracking the functions effects and constraining
-- the user (programmer) of those function to use specific effects in them.
-- https://github.com/the-blockchain-company/bcc-sl/blob/develop/util/src/Pos/Util/Log/LogSafe.hs
data LoggingLayer = LoggingLayer
  { llBasicTrace :: forall m. (MonadIO m) => Trace m Text
  , llLogDebug :: forall m a. (MonadIO m, Show a) => Trace m a -> a -> m ()
  , llLogInfo :: forall m a. (MonadIO m, Show a) => Trace m a -> a -> m ()
  , llLogNotice :: forall m a. (MonadIO m, Show a) => Trace m a -> a -> m ()
  , llLogWarning :: forall m a. (MonadIO m, Show a) => Trace m a -> a -> m ()
  , llLogError :: forall m a. (MonadIO m, Show a) => Trace m a -> a -> m ()
  , llAppendName :: forall m a. (Show a) => LoggerName -> Trace m a -> Trace m a
  , llBracketMonadIO :: forall a t. (Show a) => Trace IO a -> Severity -> Text -> IO t -> IO t
  , llBracketMonadM
      :: forall m a t. (MonadCatch m, MonadIO m, Show a)
      => Trace m a -> Severity -> Text -> m t -> m t
  , llBracketMonadX
      :: forall m a t. (MonadIO m, Show a) => Trace m a -> Severity -> Text -> m t -> m t
  , llBracketStmIO :: forall a t. (Show a) => Trace IO a -> Severity -> Text -> STM t -> IO t
  , llBracketStmLogIO
      :: forall a t. (Show a)
      => Trace IO a -> Severity -> Text -> STM (t,[(LOMeta, LOContent a)]) -> IO t
  , llConfiguration :: Configuration
  , llAddBackend :: Backend Text -> BackendKind -> IO ()
  , llSwitchboard :: Switchboard Text
  , llEKGDirect :: Maybe EKGDirect
  }

data EKGDirect = EKGDirect
  { ekgServer   :: EKG.Server
  , ekgGauges   :: MVar (Map.Map Text Gauge)
  , ekgLabels   :: MVar (Map.Map Text Label)
  , ekgCounters :: MVar (Map.Map Text Counter)
  }

--------------------------------
-- Feature
--------------------------------

-- | Either parse a filepath into a logging 'Configuration',
--   or supply a mute 'Configuration'.
loggingCLIConfiguration
    :: Maybe FilePath
    -> ExceptT ConfigError IO Configuration
loggingCLIConfiguration = maybe emptyConfig readConfig
 where
   readConfig :: FilePath -> ExceptT ConfigError IO Configuration
   readConfig fp =
     catchIOExceptT (Config.setup fp) $ \(_ :: IOException) -> ConfigErrorFileNotFound fp

   emptyConfig :: ExceptT ConfigError IO Configuration
   emptyConfig = liftIO $ do
     c <- Config.empty
     Config.setMinSeverity c Info
     pure c

-- | Create logging feature for `bcc-node`
createLoggingLayer
  :: Text
  -> NodeConfiguration
  -> SomeConsensusProtocol
  -> ExceptT ConfigError IO LoggingLayer
createLoggingLayer ver nodeConfig' p = do

  logConfig <- loggingCLIConfiguration $
    if ncLoggingSwitch nodeConfig'
    -- Re-interpret node config again, as logging 'Configuration':
    then Just . unConfigPath $ ncConfigFile nodeConfig'
    else Nothing

  -- These have to be set before the switchboard is set up.
  liftIO $ do
    Config.setTextOption logConfig "appversion" ver
    Config.setTextOption logConfig "appcommit" gitRev

  (baseTrace, switchBoard) <- liftIO $ setupTrace_ logConfig "bcc"

  let loggingEnabled :: Bool
      loggingEnabled = ncLoggingSwitch nodeConfig'
      trace :: Trace IO Text
      trace = if loggingEnabled
              then baseTrace
              else Trace.nullTracer

  when loggingEnabled $ liftIO $
    loggingPreInit nodeConfig' logConfig switchBoard trace

  mEKGServer <- liftIO $ Switchboard.getSbEKGServer switchBoard

  mbEkgDirect <- case mEKGServer of
                  Nothing -> pure Nothing
                  Just sv -> do
                    refGauge   <- liftIO $ newMVar Map.empty
                    refLabel   <- liftIO $ newMVar Map.empty
                    refCounter <- liftIO $ newMVar Map.empty
                    pure $ Just EKGDirect {
                        ekgServer   = sv
                      , ekgGauges   = refGauge
                      , ekgLabels   = refLabel
                      , ekgCounters = refCounter
                      }

  pure $ mkLogLayer logConfig switchBoard mbEkgDirect trace
 where
   loggingPreInit
     :: NodeConfiguration
     -> Configuration
     -> Switchboard Text
     -> Trace IO Text
     -> IO ()
   loggingPreInit nodeConfig logConfig switchBoard trace = do
     Config.getEKGBindAddr logConfig >>= \mbEndpoint ->
       when (isJust mbEndpoint) $
         Bcc.BM.Backend.EKGView.plugin logConfig trace switchBoard
           >>= loadPlugin switchBoard

     Config.getForwardTo logConfig >>= \forwardTo ->
       when (isJust forwardTo) $ do
         -- Since the configuration contains 'traceForwardTo' section,
         -- node's information (metrics/peers/errors) should be forwarded
         -- to an external process (for example, RTView).

         -- Activate TraceForwarder plugin (there is no need to add 'TraceForwarderBK'
         -- to 'setupBackends' list).
         nodeStartTime <- getCurrentTime
         Bcc.BM.Backend.TraceForwarder.plugin logConfig
                                                  trace
                                                  switchBoard
                                                  "forwarderMinSeverity"
                                                  (nodeBasicInfo nodeConfig p nodeStartTime)
           >>= loadPlugin switchBoard

         -- Forward all the metrics/peers/errors to 'TraceForwarderBK' using 'mapBackends'.
         -- If 'TraceForwarderBK' is already added in 'mapBackends' - ignore it.
         let metricsLogger = "bcc.node.metrics" -- All metrics and peers info are here.
             errorsLoggers = "bcc.node" -- All errors (messages with 'Warning+' severity) are here.

         forM_ [metricsLogger, errorsLoggers] $ \loggerName ->
           Config.getBackends logConfig loggerName >>= \backends ->
             when (TraceForwarderBK `notElem` backends) $
               Config.setBackends logConfig loggerName $ Just (TraceForwarderBK : backends)

     Bcc.BM.Backend.Aggregation.plugin logConfig trace switchBoard
       >>= loadPlugin switchBoard
     Bcc.BM.Backend.Monitoring.plugin logConfig trace switchBoard
       >>= loadPlugin switchBoard

#if defined(SYSTEMD)
     Bcc.BM.Scribe.Systemd.plugin logConfig trace switchBoard "bcc"
       >>= loadPlugin switchBoard
#endif

     when (ncLogMetrics nodeConfig) $
       -- Record node metrics, if configured
       startCapturingMetrics trace

   mkLogLayer :: Configuration -> Switchboard Text -> Maybe EKGDirect -> Trace IO Text -> LoggingLayer
   mkLogLayer logConfig switchBoard mbEkgDirect trace =
     LoggingLayer
       { llBasicTrace = Trace.natTrace liftIO trace
       , llLogDebug = Trace.logDebug
       , llLogInfo = Trace.logInfo
       , llLogNotice = Trace.logNotice
       , llLogWarning = Trace.logWarning
       , llLogError = Trace.logError
       , llAppendName = Trace.appendName
       , llBracketMonadIO = Monadic.bracketObserveIO logConfig
       , llBracketMonadM = Monadic.bracketObserveM logConfig
       , llBracketMonadX = Monadic.bracketObserveX logConfig
       , llBracketStmIO = Stm.bracketObserveIO logConfig
       , llBracketStmLogIO = Stm.bracketObserveLogIO logConfig
       , llConfiguration = logConfig
       , llAddBackend = Switchboard.addExternalBackend switchBoard
       , llSwitchboard = switchBoard
       , llEKGDirect = mbEkgDirect
       }

   startCapturingMetrics :: Trace IO Text -> IO ()
   startCapturingMetrics tr = do
     void . Async.async . forever $ do
       readResourceStats
         >>= maybe (pure ())
                   (traceResourceStats
                      (appendName "node" tr))
       threadDelay 1000000 -- TODO:  make configurable
   traceResourceStats :: Trace IO Text -> ResourceStats -> IO ()
   traceResourceStats tr rs = do
     traceWith (toLogObject' NormalVerbosity $ appendName "resources" tr) rs
     traceCounter "Stat.cputicks"    tr . fromIntegral $ rCentiCpu rs
     traceCounter "Mem.resident"     tr . fromIntegral $ rRSS rs
     traceCounter "RTS.gcLiveBytes"  tr . fromIntegral $ rLive rs
     traceCounter "RTS.gcHeapBytes"  tr . fromIntegral $ rHeap rs
     traceCounter "RTS.gcMajorNum"   tr . fromIntegral $ rGcsMajor rs
     traceCounter "RTS.gcMinorNum"   tr . fromIntegral $ rGcsMinor rs
     traceCounter "RTS.gcticks"      tr . fromIntegral $ rCentiGC rs
     traceCounter "RTS.mutticks"     tr . fromIntegral $ rCentiMut rs
     traceCounter "Stat.threads"     tr . fromIntegral $ rThreads rs

traceCounter
  :: Text
  -> Trace IO Text
  -> Int
  -> IO ()
traceCounter logValueName tracer aCounter = do
  meta <- mkLOMeta Notice Public
  Trace.traceNamedObject
    (appendName "metrics" tracer)
    (meta, LogValue logValueName (PureI $ fromIntegral aCounter))

shutdownLoggingLayer :: LoggingLayer -> IO ()
shutdownLoggingLayer = shutdown . llSwitchboard

-- The node provides the basic node's information for TraceForwarderBK.
-- It will be sent once TraceForwarderBK is connected to an external process
-- (for example, RTView).
nodeBasicInfo :: NodeConfiguration
              -> SomeConsensusProtocol
              -> UTCTime
              -> IO [LogObject Text]
nodeBasicInfo nc (SomeConsensusProtocol whichP pForInfo) nodeStartTime' = do
  meta <- mkLOMeta Notice Public
  let cfg = pInfoConfig $ protocolInfo pForInfo
      protocolDependentItems =
        case whichP of
          ColeBlockType ->
            let DegenLedgerConfig cfgCole = Consensus.configLedger cfg
            in getGenesisValuesCole cfg cfgCole
          SophieBlockType ->
            let DegenLedgerConfig cfgSophie = Consensus.configLedger cfg
            in getGenesisValues "Sophie" cfgSophie
          BccBlockType ->
            let BccLedgerConfig cfgCole cfgSophie cfgEvie cfgJen cfgAurum = Consensus.configLedger cfg
            in getGenesisValuesCole cfg cfgCole
               ++ getGenesisValues "Sophie" cfgSophie
               ++ getGenesisValues "Evie" cfgEvie
               ++ getGenesisValues "Jen"    cfgJen
               ++ getGenesisValues "Aurum"  cfgAurum
      items = nub $
        [ ("protocol",      pack . protocolName $ ncProtocol nc)
        , ("version",       pack . showVersion $ version)
        , ("commit",        gitRev)
        , ("nodeStartTime", show nodeStartTime')
        ] ++ protocolDependentItems
      logObjects =
        map (\(nm, msg) -> LogObject ("basicInfo." <> nm) meta (LogMessage msg)) items
  return logObjects
 where
  getGenesisValuesCole cfg config =
    let genesis = coleLedgerConfig config
    in [ ("systemStartTime",  show (WCT.getSystemStart . getSystemStart $ Consensus.configBlock cfg))
       , ("slotLengthCole",  show (WCT.getSlotLength . fromColeSlotLength $ genesisSlotLength genesis))
       , ("epochLengthCole", show (unEpochSize . fromColeEpochSlots $ Gen.configEpochSlots genesis))
       ]
  getGenesisValues era config =
    let genesis = sophieLedgerGenesis $ sophieLedgerConfig config
    in [ ("systemStartTime",          show (SL.sgSystemStart genesis))
       , ("slotLength" <> era,        show (WCT.getSlotLength . WCT.mkSlotLength $ SL.sgSlotLength genesis))
       , ("epochLength" <> era,       show (unEpochSize . SL.sgEpochLength $ genesis))
       , ("slotsPerKESPeriod" <> era, show (SL.sgSlotsPerKESPeriod genesis))
       ]
