
-- | Dispatch for running all the CLI commands
module Bcc.Unlog.Run
  ( Command(..)
  , CommandErrors
  , renderCommandError
  , runCommand
  ) where

import           Bcc.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT)
import qualified Data.Text as Text

import           Bcc.Analysis.Driver (AnalysisCmdError, renderAnalysisCmdError,
                     runAnalysisCommand)
import           Bcc.Unlog.Commands (AnalysisCommand)

import           Bcc.Config.Git.Rev (gitRev)
import           Data.Version (showVersion)
import           Paths_locli (version)

-- | Sub-commands of 'locli'.
data Command =

  -- | Analysis commands
    AnalysisCommand AnalysisCommand

  | DisplayVersion
  deriving Show

data CommandErrors
  = AnalysisError AnalysisCommand AnalysisCmdError
  deriving Show

runCommand :: Command -> ExceptT CommandErrors IO ()
runCommand (AnalysisCommand c) = firstExceptT (AnalysisError c) $ runAnalysisCommand c
runCommand DisplayVersion = runDisplayVersion

renderCommandError :: CommandErrors -> Text
renderCommandError (AnalysisError cmd err) =
  renderAnalysisCmdError cmd err

runDisplayVersion :: ExceptT CommandErrors IO ()
runDisplayVersion = do
    liftIO . putTextLn $ mconcat
                [ "locli ", renderVersion version
                , ", git rev ", gitRev
                ]
  where
    renderVersion = Text.pack . showVersion
