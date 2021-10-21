{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Bcc.Benchmarking.Script.Types
where

import           Prelude
import           GHC.Generics

import           Bcc.Benchmarking.ShardagnosticImports (SigningKeyFile)
import           Bcc.Api (AnyBccEra, ExecutionUnits, Entropic, ScriptData, ScriptRedeemer)

import           Bcc.Benchmarking.Script.Env
import           Bcc.Benchmarking.Script.Store
import           Bcc.Benchmarking.Types (TPSRate, NumberOfTxs)

data Action where
  Set                :: !SetKeyVal -> Action
--  Declare            :: SetKeyVal   -> Action --declare (once): error if key was set before
  StartProtocol      :: !FilePath -> Action
  Delay              :: !Double -> Action
  ReadSigningKey     :: !KeyName -> !SigningKeyFile -> Action
  SecureGenesisFund  :: !FundName -> !KeyName -> !KeyName -> Action
  SplitFund          :: [FundName] -> !KeyName -> !FundName -> Action
  SplitFundToList    :: !FundListName -> !KeyName -> !FundName -> Action
  PrepareTxList      :: !TxListName -> !KeyName -> !FundListName -> Action
  AsyncBenchmark     :: !ThreadName -> !TxListName -> !TPSRate -> Action
  ImportGenesisFund  :: !SubmitMode -> !KeyName -> !KeyName -> Action
  CreateChange       :: !SubmitMode -> !PayMode -> !Entropic -> !Int -> Action
  RunBenchmark       :: !SubmitMode -> !SpendMode -> !ThreadName -> !NumberOfTxs -> !TPSRate -> Action
  WaitBenchmark      :: !ThreadName -> Action
  CancelBenchmark    :: !ThreadName -> Action
  Reserved           :: [String] -> Action
  WaitForEra         :: !AnyBccEra -> Action
  deriving (Show, Eq)
deriving instance Generic Action

data SubmitMode where
  LocalSocket :: SubmitMode
  NodeToNode  :: SubmitMode
  DumpToFile  :: !FilePath -> SubmitMode
  DiscardTX   :: SubmitMode
  deriving (Show, Eq)
deriving instance Generic SubmitMode

data PayMode where
  PayToAddr :: PayMode
  PayToCollateral :: PayMode  
  PayToScript :: !String -> !ScriptData -> PayMode
  deriving (Show, Eq)
deriving instance Generic PayMode

data SpendMode where
  SpendOutput :: SpendMode
  SpendScript :: !String -> !ExecutionUnits -> !ScriptData -> !ScriptRedeemer -> SpendMode
  deriving (Show, Eq)
deriving instance Generic SpendMode
