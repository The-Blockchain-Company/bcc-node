{-# LANGUAGE NamedFieldPuns #-}

-- | This top-level module will be used by the acceptor application.
--   Acceptor application asks 'TraceObject's from the forwarder application.
module Trace.Forward.Acceptor
  ( runTraceAcceptor
  ) where

import qualified Codec.Serialise as CBOR
import           Data.Typeable (Typeable)

import           Shardagnostic.Network.IOManager (IOManager)
import           Shardagnostic.Network.Util.ShowProxy (ShowProxy(..))

import           Trace.Forward.Network.Acceptor (listenToForwarder)
import           Trace.Forward.Configuration (AcceptorConfiguration (..))
import           Trace.Forward.Protocol.Type (NodeInfo)
import           Trace.Forward.Utils (runActionInLoop)

runTraceAcceptor
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  => IOManager                -- ^ 'IOManager' from the external application.
  -> AcceptorConfiguration lo -- ^ Acceptor configuration.
  -> ([lo] -> IO ())          -- ^ The handler for 'TraceObject's received from the node.
  -> (NodeInfo -> IO ())      -- ^ The handler for node's info received from the node.
  -> IO ()
runTraceAcceptor iomgr config@AcceptorConfiguration{forwarderEndpoint} loHandler niHandler =
  runActionInLoop (listenToForwarder iomgr config loHandler niHandler) forwarderEndpoint 1
