{-# LANGUAGE OverloadedStrings #-}

module Bcc.CLI.Environment
  ( EnvSocketError(..)
  , readEnvSocketPath
  , renderEnvSocketError
  ) where

import           Bcc.Prelude
import           Prelude (String)

import           Control.Monad.Trans.Except.Extra (left)
import qualified Data.Text as Text
import           System.Environment (lookupEnv)

import           Bcc.CLI.Helpers (textShow)
import           Bcc.CLI.Types (SocketPath (..))

newtype EnvSocketError = CliEnvVarLookup Text deriving Show

renderEnvSocketError :: EnvSocketError -> Text
renderEnvSocketError err =
  case err of
    CliEnvVarLookup txt ->
      "Error while looking up environment variable: BCC_NODE_SOCKET_PATH " <> " Error: " <> textShow txt

-- | Read the node socket path from the environment.
-- Fails if the environment variable is not set.
readEnvSocketPath :: ExceptT EnvSocketError IO SocketPath
readEnvSocketPath =
    maybe (left $ CliEnvVarLookup (Text.pack envName)) (pure . SocketPath)
      =<< liftIO (lookupEnv envName)
  where
    envName :: String
    envName = "BCC_NODE_SOCKET_PATH"
