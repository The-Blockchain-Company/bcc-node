{-# LANGUAGE ScopedTypeVariables #-}

module Bcc.Logging.Test.Tracer (
    testTracer
  ) where

import           Data.IORef
import           Control.Monad.IO.Class

import           Bcc.Logging

testTracer :: MonadIO m
  => IORef [FormattedMessage]
  -> m (Trace m FormattedMessage)
testTracer ioRef = liftIO $
    pure $ Trace $ arrow $ emit output
  where
    output (LoggingContext{}, Nothing, msg) = liftIO $ do
      modifyIORef ioRef (msg :)
    output (LoggingContext{}, _, _) = pure ()
