{-# LANGUAGE ScopedTypeVariables #-}

module Bcc.Logging.Tracer.Composed (
    mkBccTracer
  , mkBccTracer'
  , MessageOrLimit(..)
  ) where

import           Data.Maybe                       (fromMaybe)
import           Data.Text

import           Bcc.Logging.Configuration
import           Bcc.Logging.Formatter
import           Bcc.Logging.FrequencyLimiter (LimitingMessage (..))
import           Bcc.Logging.Trace
import           Bcc.Logging.Types

import qualified Control.Tracer                   as NT

data MessageOrLimit m = Message m | Limit LimitingMessage

instance (LogFormatting m) => LogFormatting (MessageOrLimit m) where
  forMachine dtal (Message m) = forMachine dtal m
  forMachine dtal (Limit m)   = forMachine dtal m
  forHuman (Message m) = forHuman m
  forHuman (Limit m)   = forHuman m
  asMetrics (Message m) = asMetrics m
  asMetrics (Limit m)   = asMetrics m

-- | Construct a tracer according to the requirements for bcc node.
--
-- The tracer gets a 'name', which is appended to its namespace.
--
-- The tracer gets a 'namesFor', 'severityFor' and 'privacyFor' function
-- as arguments, to set the logging context accordingly.
--
-- The tracer gets the backends: 'trStdout', 'trForward' and 'mbTrEkg'
-- as arguments.
--
-- The returned tracer need to be configured for the specification of
-- filtering, detailLevel, frequencyLimiting and backends with formatting before use.
mkBccTracer :: forall evt.
     LogFormatting evt
  => Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> Text
  -> (evt -> [Text])
  -> (evt -> SeverityS)
  -> (evt -> Privacy)
  -> IO (Trace IO evt)
mkBccTracer trStdout trForward mbTrEkg name namesFor severityFor privacyFor =
    mkBccTracer' trStdout trForward mbTrEkg name namesFor severityFor
        privacyFor noHook
  where
    noHook :: Trace IO evt -> IO (Trace IO evt)
    noHook = pure

-- | Adds the possibility to add special tracers via the hook function
mkBccTracer' :: forall evt evt1.
     LogFormatting evt1
  => Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> Text
  -> (evt -> [Text])
  -> (evt -> SeverityS)
  -> (evt -> Privacy)
  -> (Trace IO evt1 -> IO (Trace IO evt))
  -> IO (Trace IO evt)
mkBccTracer' trStdout trForward mbTrEkg name namesFor severityFor privacyFor
  hook = do
    tr    <- withBackendsFromConfig backendsAndFormat
    tr'   <- withLimitersFromConfig (NT.contramap Message tr) (NT.contramap Limit tr)
    tr''  <- hook tr'
    addContextAndFilter tr''
  where
    addContextAndFilter :: Trace IO evt -> IO (Trace IO evt)
    addContextAndFilter tr = do
      tr'  <- withDetailsFromConfig tr
      tr'' <- filterSeverityFromConfig tr'
      pure $ withNamesAppended namesFor
            $ appendName name
              $ appendName "Node"
                $ withSeverity severityFor
                  $ withPrivacy privacyFor
                    tr''

    backendsAndFormat ::
         Maybe [BackendConfig]
      -> Trace m x
      -> IO (Trace IO (MessageOrLimit evt1))
    backendsAndFormat mbBackends _ =
      let backends = fromMaybe
                      [EKGBackend, Forwarder, Stdout HumanFormatColoured]
                      mbBackends
      in do
        mbEkgTrace     <- case mbTrEkg of
                            Nothing -> pure Nothing
                            Just ekgTrace ->
                              if EKGBackend `elem` backends
                                then pure $ Just
                                      (metricsFormatter "Bcc" ekgTrace)
                                else pure Nothing
        mbForwardTrace <- if Forwarder `elem` backends
                            then fmap (Just . filterTraceByPrivacy (Just Public))
                                  (forwardFormatter "Bcc" trForward)
                            else pure Nothing
        mbStdoutTrace  <-  if Stdout HumanFormatColoured `elem` backends
                            then fmap Just
                                (humanFormatter True "Bcc" trStdout)
                            else if Stdout HumanFormatUncoloured `elem` backends
                              then fmap Just
                                  (humanFormatter False "Bcc" trStdout)
                              else if Stdout MachineFormat `elem` backends
                                then fmap Just
                                  (machineFormatter "Bcc" trStdout)
                                else pure Nothing
        case mbEkgTrace <> mbForwardTrace <> mbStdoutTrace of
          Nothing -> pure $ Trace NT.nullTracer
          Just tr -> pure (preFormatted backends tr)
