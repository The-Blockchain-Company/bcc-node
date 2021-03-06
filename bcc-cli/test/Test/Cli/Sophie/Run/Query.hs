module Test.Cli.Sophie.Run.Query
  ( tests
  ) where

import           Bcc.Slotting.Time (RelativeTime(..))
import           Control.Monad (return)
import           Data.Bool
import           Data.Function
import           Hedgehog (Property, (===))
import           System.IO as IO

import qualified Bcc.CLI.Sophie.Run.Query as Q
import qualified Hedgehog as H 
import qualified Hedgehog.Extras.Test.Base as H

unit_percentage :: Property
unit_percentage = H.propertyOnce $ do
  Q.percentage (RelativeTime 10) (RelativeTime 1000) (RelativeTime 1000) === "100.00"
  Q.percentage (RelativeTime 10) (RelativeTime 990) (RelativeTime 1000) === "100.00"
  Q.percentage (RelativeTime 10) (RelativeTime 980) (RelativeTime 1000) === "99.00"
  Q.percentage (RelativeTime 10) (RelativeTime 500) (RelativeTime 1000) === "51.05"
  Q.percentage (RelativeTime 10) (RelativeTime 0) (RelativeTime 1000) === "1.10"
  return ()

tests :: IO Bool
tests =
  H.checkSequential
    $ H.Group "Pioneers Example 6"
        [ ("prop_createZeroEntropicTxOutTransaction", unit_percentage)
        ]
