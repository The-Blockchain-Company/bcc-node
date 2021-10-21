module Bcc.Benchmarking.Script.Action
where

import           Data.Functor.Identity
import           Data.Dependent.Sum (DSum(..))

import           Bcc.Benchmarking.Script.Env
import           Bcc.Benchmarking.Script.Store
import           Bcc.Benchmarking.Script.Core
import           Bcc.Benchmarking.Script.Types

action :: Action -> ActionM ()
action a = case a of
  Set (key :=> (Identity val)) -> set (User key) val
  StartProtocol filePath -> startProtocol filePath
  ReadSigningKey name filePath -> readSigningKey name filePath
  SecureGenesisFund fundName fundKey genesisKey -> secureGenesisFund fundName fundKey genesisKey
  SplitFund newFunds newKey sourceFund -> splitFund  newFunds newKey sourceFund
  SplitFundToList fundList destKey sourceFund -> splitFundToList fundList destKey sourceFund
  Delay t -> delay t
  PrepareTxList name key fund -> prepareTxList name key fund
  AsyncBenchmark thread txs tps -> asyncBenchmark thread txs tps
  ImportGenesisFund submitMode genesisKey fundKey -> importGenesisFund submitMode genesisKey fundKey
  CreateChange payMode submitMode value count -> createChange payMode submitMode value count
  RunBenchmark submitMode spendMode thread count tps -> runBenchmark submitMode spendMode thread count tps
  WaitBenchmark thread -> waitBenchmark thread
  CancelBenchmark thread -> cancelBenchmark thread
  WaitForEra era -> waitForEra era
  Reserved options -> reserved options
