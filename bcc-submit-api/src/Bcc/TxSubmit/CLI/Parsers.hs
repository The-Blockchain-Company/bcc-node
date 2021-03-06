{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Bcc.TxSubmit.CLI.Parsers
  ( opts
  , pTxSubmitNodeParams
  , pConfigFile
  , pNetworkId
  , pProtocol
  , pSocketPath
  ) where

import           Bcc.Api (AnyConsensusModeParams (..), ConsensusModeParams (..),
                   EpochSlots (..), NetworkId (..), NetworkMagic (..))
import           Bcc.TxSubmit.CLI.Types (ConfigFile (..), SocketPath (..),
                   TxSubmitNodeParams (..))
import           Bcc.TxSubmit.Rest.Parsers (pWebserverConfig)
import           Control.Applicative (Alternative (..), Applicative (..), (<**>))
import           Data.Function ((.))
import           Data.Functor (Functor (fmap), (<$>))
import           Data.Semigroup (Semigroup ((<>)))
import           Data.Word (Word64)
import           Options.Applicative (Parser, ParserInfo)

import qualified Options.Applicative as Opt

opts :: ParserInfo TxSubmitNodeParams
opts = Opt.info (pTxSubmitNodeParams <**> Opt.helper)
  (   Opt.fullDesc
  <>  Opt.progDesc "Bcc transaction submission web API."
  )

pTxSubmitNodeParams :: Parser TxSubmitNodeParams
pTxSubmitNodeParams = TxSubmitNodeParams
  <$> pConfigFile
  <*> pProtocol
  <*> pNetworkId
  <*> pSocketPath
  <*> pWebserverConfig 8090

pConfigFile :: Parser ConfigFile
pConfigFile = ConfigFile <$> Opt.strOption
  (   Opt.long "config"
  <>  Opt.help "Path to the tx-submit web API configuration file"
  <>  Opt.completer (Opt.bashCompleter "file")
  <>  Opt.metavar "FILEPATH"
  )

-- TODO: This was ripped from `bcc-cli` because, unfortunately, it's not
-- exported. Once we export this parser from the appropriate module and update
-- our `bcc-cli` dependency, we should remove this and import the parser
-- from there.
pNetworkId :: Parser NetworkId
pNetworkId = pMainnet <|> fmap Testnet pTestnetMagic
  where
    pMainnet :: Parser NetworkId
    pMainnet = Opt.flag' Mainnet
      (   Opt.long "mainnet"
      <>  Opt.help "Use the mainnet magic id."
      )

    pTestnetMagic :: Parser NetworkMagic
    pTestnetMagic = NetworkMagic <$> Opt.option Opt.auto
      (   Opt.long "testnet-magic"
      <>  Opt.metavar "NATURAL"
      <>  Opt.help "Specify a testnet magic id."
      )


-- TODO: This was ripped from `bcc-cli` because, unfortunately, it's not
-- exported. Once we export this parser from the appropriate module and update
-- our `bcc-cli` dependency, we should remove this and import the parser
-- from there.
pProtocol :: Parser AnyConsensusModeParams
pProtocol =
      ( Opt.flag' ()
        (   Opt.long "sophie-mode"
        <>  Opt.help "For talking to a node running in Sophie-only mode."
        )
        *> pSophie
      )
  <|> ( Opt.flag' ()
        (   Opt.long "cole-mode"
        <>  Opt.help "For talking to a node running in Cole-only mode."
        )
        *> pCole
      )
  <|> ( Opt.flag' ()
        (   Opt.long "bcc-mode"
        <>  Opt.help "For talking to a node running in full Bcc mode (default)."
        )
        *> pBcc
      )
  <|> -- Default to the Bcc protocol.
      pure (AnyConsensusModeParams (BccModeParams (EpochSlots defaultColeEpochSlots)))
  where
    pCole :: Parser AnyConsensusModeParams
    pCole = AnyConsensusModeParams . ColeModeParams <$> pEpochSlots

    pSophie :: Parser AnyConsensusModeParams
    pSophie = pure (AnyConsensusModeParams SophieModeParams)

    pBcc :: Parser AnyConsensusModeParams
    pBcc = AnyConsensusModeParams . BccModeParams <$> pEpochSlots

    pEpochSlots :: Parser EpochSlots
    pEpochSlots = EpochSlots <$> Opt.option Opt.auto
      (   Opt.long "epoch-slots"
      <>  Opt.metavar "NATURAL"
      <>  Opt.help "The number of slots per epoch for the Cole era."
      <>  Opt.value defaultColeEpochSlots -- Default to the mainnet value.
      <>  Opt.showDefault
      )

    defaultColeEpochSlots :: Word64
    defaultColeEpochSlots = 21600

pSocketPath :: Parser SocketPath
pSocketPath = SocketPath <$> Opt.strOption
  (   Opt.long "socket-path"
  <>  Opt.help "Path to a bcc-node socket"
  <>  Opt.completer (Opt.bashCompleter "file")
  <>  Opt.metavar "FILEPATH"
  )
