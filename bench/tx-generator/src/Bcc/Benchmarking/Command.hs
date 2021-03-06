{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations -Wno-orphans #-}

module Bcc.Benchmarking.Command
(
  runCommand
, commandParser -- for tests
)
where

import           Prelude
import           System.Exit

import           Options.Applicative as Opt

import           Shardagnostic.Network.NodeToClient (withIOManager)

import           Bcc.Benchmarking.CliArgsScript
                   (GeneratorCmd, parseGeneratorCmd, runPlainOldCliScript, runEraTransitionTest)
import           Bcc.Benchmarking.Script (runScript, parseScriptFile)

data Command
  = CliArguments !GeneratorCmd
  | EraTransition !GeneratorCmd
  | Json !FilePath

runCommand :: IO ()
runCommand = withIOManager $ \iocp -> do
  cmd <- customExecParser
           (prefs showHelpOnEmpty)
           (info commandParser mempty)
  case cmd of
    CliArguments   args -> runPlainOldCliScript iocp args >>= handleError
    EraTransition args -> runEraTransitionTest iocp args >>= handleError
    Json file     -> do
      script <- parseScriptFile file
      runScript script iocp >>= handleError
 where
  handleError :: Show a => Either a b -> IO ()
  handleError = \case
    Right _  -> exitSuccess
    Left err -> die $ show err

commandParser :: Parser Command
commandParser
  = subparser
    (  cliArgumentsCmd
    <> eraTransitionCmd
    <> jsonCmd
    )
 where
  cliArgumentsCmd = command "cliArguments"
    (CliArguments <$> info parseGeneratorCmd
      (  progDesc "tx-generator with CLI arguments"
      <> fullDesc
      <> header "tx-generator - load Bcc clusters with parametrised transaction flow (CLI version)"
      )
    )

  eraTransitionCmd = command "eraTransition"
    (EraTransition <$> info parseGeneratorCmd
      (  progDesc "tx-generator demo era transition"
      <> fullDesc
      <> header "tx-generator - load Bcc clusters with parametrised transaction flow (era transition)"
      )
    )

  jsonCmd = command "json"
    (Json <$> info (strArgument (metavar "FILEPATH"))
      (  progDesc "tx-generator run JsonScript"
      <> fullDesc
      <> header "tx-generator - run a generic benchmarking script"
      )
    )
