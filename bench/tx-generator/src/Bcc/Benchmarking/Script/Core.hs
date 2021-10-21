{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use uncurry" -}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-} --
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Bcc.Benchmarking.Script.Core
where

import           Prelude
import           Data.Ratio ((%))
import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Control.Concurrent (threadDelay)
import           Control.Tracer (traceWith, nullTracer)

import           Shardagnostic.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))
import           Bcc.Api
import           Bcc.Api.Sophie ( ProtocolParameters, protocolParamPrices)

import qualified Bcc.Benchmarking.FundSet as FundSet
import           Bcc.Benchmarking.FundSet (FundInEra(..), Validity(..), Variant(..), liftAnyEra )
import qualified Bcc.Benchmarking.GeneratorTx as GeneratorTx
                   (asyncBenchmark, waitBenchmark, walletBenchmark
                   , readSigningKey, secureGenesisFund, splitFunds, txGenerator)
import           Bcc.Benchmarking.GeneratorTx as GeneratorTx
                   (AsyncBenchmarkControl, TxGenError)

import           Bcc.Benchmarking.GeneratorTx.LocalProtocolDefinition as Core (startProtocol)
import           Bcc.Benchmarking.GeneratorTx.NodeToNode (ConnectClient, benchmarkConnectTxSubmit)
import           Bcc.Benchmarking.GeneratorTx.SizedMetadata (mkMetadata)
import           Bcc.Benchmarking.GeneratorTx.Tx as Core (keyAddress, mkFee, txInModeBcc)

import           Bcc.Benchmarking.ShardagnosticImports as Core
                   (LocalSubmitTx, SigningKeyFile
                   , getGenesis, protocolToNetworkId, protocolToCodecConfig, makeLocalConnectInfo)
import           Bcc.Benchmarking.ZerepochExample as ZerepochExample
import           Bcc.Benchmarking.Tracer as Core
                   ( TraceBenchTxSubmit (..)
                   , createTracers, btTxSubmit_, btN2N_, btConnect_, btSubmission2_)
import           Bcc.Benchmarking.Types as Core
                   (NumberOfInputsPerTx(..), NumberOfOutputsPerTx(..),NumberOfTxs(..), SubmissionErrorPolicy(..)
                   , TPSRate, TxAdditionalSize(..))
import           Bcc.Benchmarking.Wallet as Wallet
import           Bcc.Benchmarking.ListBufferedSelector

import           Bcc.Benchmarking.Script.Env
import           Bcc.Benchmarking.Script.Setters
import           Bcc.Benchmarking.Script.Store as Store
import           Bcc.Benchmarking.Script.Types

liftCoreWithEra :: (forall era. IsSophieBasedEra era => AsType era -> ExceptT TxGenError IO x) -> ActionM (Either TxGenError x)
liftCoreWithEra coreCall = withEra ( liftIO . runExceptT . coreCall)

withEra :: (forall era. IsSophieBasedEra era => AsType era -> ActionM x) -> ActionM x
withEra action = do
  era <- get $ User TEra
  case era of
    AnyBccEra AurumEra  -> action AsAurumEra
    AnyBccEra JenEra    -> action AsJenEra
    AnyBccEra EvieEra -> action AsEvieEra
    AnyBccEra SophieEra -> action AsSophieEra
    AnyBccEra ColeEra   -> error "cole not supported"

startProtocol :: FilePath -> ActionM ()
startProtocol filePath = do
  liftIO (runExceptT $ Core.startProtocol filePath) >>= \case
    Left err -> throwE $ CliError err
    Right (loggingLayer, protocol) -> do
      set LoggingLayer loggingLayer
      set Protocol protocol
      set BenchTracers $ Core.createTracers loggingLayer
      set Genesis $ Core.getGenesis protocol
      set NetworkId $ protocolToNetworkId protocol

readSigningKey :: KeyName -> SigningKeyFile -> ActionM ()
readSigningKey name filePath =
  liftIO ( runExceptT $ GeneratorTx.readSigningKey filePath) >>= \case
    Left err -> liftTxGenError err
    Right key -> setName name key

getLocalSubmitTx :: ActionM LocalSubmitTx
getLocalSubmitTx = submitTxToNodeLocal <$> getLocalConnectInfo

--obsolete use importGenesisFund
secureGenesisFund
   :: FundName
   -> KeyName
   -> KeyName
   -> ActionM ()
secureGenesisFund fundName destKey genesisKeyName = do
  tracer <- btTxSubmit_ <$> get BenchTracers
  localSubmit <- getLocalSubmitTx
  networkId <- get NetworkId
  genesis  <- get Genesis
  fee      <- getUser TFee
  ttl      <- getUser TTTL
  fundKey  <- getName destKey
  genesisKey  <- getName genesisKeyName
  let
    coreCall :: forall era. IsSophieBasedEra era => AsType era -> ExceptT TxGenError IO Store.Fund
    coreCall _proxy = do
      let addr = Core.keyAddress @ era networkId fundKey
      f <- GeneratorTx.secureGenesisFund tracer localSubmit networkId genesis fee ttl genesisKey addr
      return (f, fundKey)
  liftCoreWithEra coreCall >>= \case
    Left err -> liftTxGenError err
    Right fund -> do
      -- Todo : user only of two methods
      setName fundName fund -- Old method

splitFundN
   :: NumberOfTxs
   -> KeyName
   -> FundName
   -> ActionM [Store.Fund]
splitFundN count destKeyName sourceFund = do
  tracer <- btTxSubmit_ <$> get BenchTracers
  localSubmit <- getLocalSubmitTx
  networkId <- get NetworkId
  fee      <- getUser TFee
  destKey  <- getName destKeyName
  (fund, fundKey) <- consumeName sourceFund
  txIn     <- getUser TNumberOfInputsPerTx
  let
    coreCall :: forall era. IsSophieBasedEra era => AsType era -> ExceptT TxGenError IO [Store.Fund]
    coreCall _proxy = do
      let addr = Core.keyAddress @ era networkId fundKey
      f <- GeneratorTx.splitFunds tracer localSubmit fee count txIn fundKey addr fund
      return $ zip f $ repeat destKey
  liftCoreWithEra coreCall >>= \case
    Left err -> liftTxGenError err
    Right funds -> return funds

splitFund
   :: [FundName]
   -> KeyName
   -> FundName
   -> ActionM ()
splitFund newFunds destKey sourceFund = do
  funds <- splitFundN (NumberOfTxs $ fromIntegral $ length newFunds) destKey sourceFund
  forM_ (zip newFunds funds) $ \(name, f) -> setName name f

splitFundToList
   :: FundListName
   -> KeyName
   -> FundName
   -> ActionM ()
splitFundToList newFunds destKey sourceFund = do
  count <- getUser TNumberOfTxs
  funds <- splitFundN count destKey sourceFund
  setName newFunds funds

delay :: Double -> ActionM ()
delay t = liftIO $ threadDelay $ floor $ 1000000 * t

prepareTxList
   :: TxListName
   -> KeyName
   -> FundListName
   -> ActionM ()
prepareTxList name destKey srcFundName = do
  tracer   <- btTxSubmit_ <$> get BenchTracers
  networkId <- get NetworkId
  fee      <- getUser TFee
  fundList <- consumeName srcFundName
  key      <- getName destKey
  txIn     <- getUser TNumberOfInputsPerTx
  txOut    <- getUser TNumberOfOutputsPerTx
  count    <- getUser TNumberOfTxs
  payload  <- getUser TTxAdditionalSize
  let
    coreCall :: forall era. IsSophieBasedEra era => AsType era -> ExceptT TxGenError IO (InAnyBccEra TxList)
    coreCall _proxy = do
      let addr = Core.keyAddress @ era networkId key
      ----------------------------------------------------TODO : Constant 1 ???
      l <- GeneratorTx.txGenerator tracer fee count txIn txOut payload addr (snd $ head fundList) 1 (map fst fundList)
      return $ InAnyBccEra bccEra $ TxList l
  liftCoreWithEra coreCall >>= \case
    Left err -> liftTxGenError err
    Right l -> setName name l

waitBenchmarkCore :: AsyncBenchmarkControl ->  ActionM ()
waitBenchmarkCore ctl = do
  tracers  <- get BenchTracers
  _ <- liftIO $ runExceptT $ GeneratorTx.waitBenchmark (btTxSubmit_ tracers) ctl
  return ()

getConnectClient :: ActionM ConnectClient
getConnectClient = do
  tracers  <- get BenchTracers
  (Testnet networkMagic) <- get NetworkId
  protocol <- get Protocol
  void $ return $(btSubmission2_ tracers)
  ioManager <- askIOManager
  return $ benchmarkConnectTxSubmit
                       ioManager
                       (btConnect_ tracers)
                       nullTracer -- (btSubmission2_ tracers)
                       (protocolToCodecConfig protocol)
                       networkMagic

-- This the benchmark based on transaction lists.
-- It is obsolte when the tx-list are replaced with the wallet data type.
asyncBenchmarkCore :: ThreadName -> TxListName -> TPSRate -> ActionM AsyncBenchmarkControl
asyncBenchmarkCore (ThreadName threadName) transactions tps = do
  tracers  <- get BenchTracers
  txs      <- getName transactions
  targets  <- getUser TTargets
  connectClient <- getConnectClient
  let
    coreCall :: forall era. IsSophieBasedEra era => [Tx era] -> ExceptT TxGenError IO AsyncBenchmarkControl
    coreCall l = GeneratorTx.asyncBenchmark (btTxSubmit_ tracers) (btN2N_ tracers) connectClient threadName targets tps LogErrors l
  ret <- liftIO $ runExceptT $ case txs of
    InAnyBccEra AurumEra  (TxList l) -> coreCall l
    InAnyBccEra JenEra    (TxList l) -> coreCall l
    InAnyBccEra EvieEra (TxList l) -> coreCall l
    InAnyBccEra SophieEra (TxList l) -> coreCall l
    InAnyBccEra ColeEra   _ -> error "cole not supported"
  case ret of
    Left err -> liftTxGenError err
    Right ctl -> return ctl

asyncBenchmark :: ThreadName -> TxListName -> TPSRate -> ActionM ()
asyncBenchmark controlName txList tps = asyncBenchmarkCore controlName txList tps >>= setName controlName

waitBenchmark :: ThreadName -> ActionM ()
waitBenchmark n = getName n >>= waitBenchmarkCore

cancelBenchmark :: ThreadName -> ActionM ()
cancelBenchmark n = do
  ctl@(_, _ , _ , shutdownAction) <- getName n
  liftIO shutdownAction
  waitBenchmarkCore ctl

getLocalConnectInfo :: ActionM  (LocalNodeConnectInfo BccMode)
getLocalConnectInfo = makeLocalConnectInfo <$> get NetworkId <*> getUser TLocalSocket

queryEra :: ActionM AnyBccEra
queryEra = do
  localNodeConnectInfo <- getLocalConnectInfo
  chainTip  <- liftIO $ getLocalChainTip localNodeConnectInfo
  ret <- liftIO $ queryNodeLocalState localNodeConnectInfo (Just $ chainTipToChainPoint chainTip) $ QueryCurrentEra BccModeIsMultiEra
  case ret of
    Right era -> return era
    Left err -> throwE $ ApiError $ show err

queryProtocolParameters :: ActionM ProtocolParameters
queryProtocolParameters = do
  localNodeConnectInfo <- getLocalConnectInfo
  chainTip  <- liftIO $ getLocalChainTip localNodeConnectInfo
  ret <- liftIO $ queryNodeLocalState localNodeConnectInfo (Just $ chainTipToChainPoint chainTip)
                    $ QueryInEra AurumEraInBccMode $ QueryInSophieBasedEra SophieBasedEraAurum QueryProtocolParameters
  case ret of
    Right (Right pp) -> return pp
    Right (Left err) -> throwE $ ApiError $ show err
    Left err -> throwE $ ApiError $ show err

waitForEra :: AnyBccEra -> ActionM ()
waitForEra era = do
  currentEra <- queryEra
  if currentEra == era
    then return ()
    else do
      traceError $ "Current era: " ++ show currentEra ++ " Waiting for: " ++ show era
      liftIO $ threadDelay 1_000_000
      waitForEra era

runWalletScriptInMode :: forall era.
     IsSophieBasedEra era
  => SubmitMode
  -> WalletScript era
  -> ActionM ()
runWalletScriptInMode submitMode s = do
  step <- liftIO $ runWalletScript s
  case step of
    Done -> return ()
    Error err -> throwE $ ApiError $ show err
    NextTx nextScript tx -> do
      case submitMode of
        LocalSocket -> void $ localSubmitTx $ txInModeBcc tx
        NodeToNode -> throwE $ ApiError "NodeToNodeMode not supported in runWalletScriptInMode"
        DumpToFile filePath -> dumpToFile filePath $ txInModeBcc tx
        DiscardTX -> return ()
      runWalletScriptInMode submitMode nextScript

localSubmitTx :: TxInMode BccMode -> ActionM (SubmitResult (TxValidationErrorInMode BccMode))
localSubmitTx tx = do
  submitTracer <- btTxSubmit_ <$> get BenchTracers
  submit <- getLocalSubmitTx
  ret <- liftIO $ submit tx
  case ret of
    SubmitSuccess -> return ()
    SubmitFail e -> liftIO $ traceWith submitTracer $
                      TraceBenchTxSubDebug $ mconcat
                        [ "local submit failed: " , show e , " (" , show tx , ")"]
  return ret

makeMetadata :: forall era. IsSophieBasedEra era => ActionM (TxMetadataInEra era)
makeMetadata = do
  payloadSize <- getUser TTxAdditionalSize
  case mkMetadata $ unTxAdditionalSize payloadSize of
    Right m -> return m
    Left err -> throwE $ MetadataError err

runBenchmark :: SubmitMode -> SpendMode -> ThreadName -> NumberOfTxs -> TPSRate -> ActionM ()
runBenchmark submitMode spendMode threadName txCount tps
  = case spendMode of
      SpendOutput -> withEra $ runBenchmarkInEra submitMode threadName txCount tps
      SpendScript scriptFile executionUnits scriptData scriptRedeemer
        -> runZerepochBenchmark submitMode scriptFile executionUnits scriptData scriptRedeemer threadName txCount tps


runBenchmarkInEra :: forall era. IsSophieBasedEra era => SubmitMode -> ThreadName -> NumberOfTxs -> TPSRate -> AsType era -> ActionM ()
runBenchmarkInEra submitMode (ThreadName threadName) txCount tps era = do
  tracers  <- get BenchTracers
  networkId <- get NetworkId
  fundKey <- getName $ KeyName "pass-partout" -- should be walletkey
  targets  <- getUser TTargets
  (NumberOfInputsPerTx   numInputs) <- getUser TNumberOfInputsPerTx
  (NumberOfOutputsPerTx numOutputs) <- getUser TNumberOfOutputsPerTx
  fee <- getUser TFee
  minValuePerUTxO <- getUser TMinValuePerUTxO
  walletRef <- get GlobalWallet
  metadata <- makeMetadata
  connectClient <- getConnectClient
  let
    (Quantity minValue) = entropicToQuantity $ fromIntegral numOutputs * minValuePerUTxO + fee

  -- this is not totally correct:
  -- beware of rounding errors !
    minValuePerInput = quantityToEntropic $ fromIntegral (if m==0 then d else d+1)
      where
        (d, m) = minValue `divMod` fromIntegral numInputs

--    fundSource :: FundSet.Target -> FundSet.FundSource
--    fundSource target = mkWalletFundSource walletRef $ FundSet.selectInputs ConfirmedBeforeReuse numInputs minTxValue PlainOldFund target

  fundSource <- liftIO (mkBufferedSource walletRef
                   (fromIntegral (unNumberOfTxs txCount) * numInputs)
                   minValuePerInput
                   PlainOldFund numInputs) >>= \case
    Right a  -> return a
    Left err -> throwE $ WalletError err

  let
    inToOut :: [Entropic] -> [Entropic]
    inToOut = FundSet.inputsToOutputsWithFee fee numOutputs

    txGenerator = genTx (mkFee fee) metadata

    toUTxO :: FundSet.Target -> FundSet.SeqNumber -> ToUTxO era
    toUTxO target seqNumber = Wallet.mkUTxO networkId fundKey (InFlight target seqNumber)

    fundToStore = mkWalletFundStore walletRef

    walletScript :: FundSet.Target -> WalletScript era
    walletScript = benchmarkWalletScript walletRef txGenerator txCount (const fundSource) inToOut toUTxO fundToStore

    coreCall :: AsType era -> ExceptT TxGenError IO AsyncBenchmarkControl
    coreCall eraProxy = GeneratorTx.walletBenchmark (btTxSubmit_ tracers) (btN2N_ tracers) connectClient
                                               threadName targets tps LogErrors eraProxy txCount walletScript
  case submitMode of
    NodeToNode -> do
      ret <- liftIO $ runExceptT $ coreCall era
      case ret of
        Left err -> liftTxGenError err
        Right ctl -> setName (ThreadName threadName) ctl
    _otherwise -> runWalletScriptInMode submitMode $ walletScript $ FundSet.Target "alternate-submit-mode"

runZerepochBenchmark :: SubmitMode -> FilePath -> ExecutionUnits -> ScriptData -> ScriptRedeemer -> ThreadName -> NumberOfTxs -> TPSRate -> ActionM ()
runZerepochBenchmark submitMode scriptFile executionUnits scriptData scriptRedeemer (ThreadName threadName) txCount tps = do
  tracers  <- get BenchTracers
  targets  <- getUser TTargets
  (NumberOfInputsPerTx   numInputs) <- getUser TNumberOfInputsPerTx
  (NumberOfOutputsPerTx numOutputs) <- getUser TNumberOfOutputsPerTx
  networkId <- get NetworkId
  minValuePerUTxO <- getUser TMinValuePerUTxO
  protocolParameters <- queryProtocolParameters
  executionUnitPrices <- case protocolParamPrices protocolParameters of
    Just x -> return x
    Nothing -> throwE $ WalletError "unexpected protocolParamPrices == Nothing in runZerepochBenchmark"
  walletRef <- get GlobalWallet
  fundKey <- getName $ KeyName "pass-partout"
  (ZerepochScript ZerepochScriptV1 script) <- liftIO $ ZerepochExample.readScript scriptFile
  -- This does not remove the collateral from the wallet, i.e. same collateral is uses for everything.
  -- This is fine unless a script ever fails.
  collateral <- liftIO ( askWalletRef walletRef (FundSet.selectCollateral . walletFunds)) >>= \case
    Right c -> return c
    Left err -> throwE $ WalletError err
  baseFee <- getUser TFee
  _minValuePerUTxO <- getUser TMinValuePerUTxO -- TODO:Fix
  metadata <- makeMetadata
  connectClient <- getConnectClient

  let
    scriptFee = quantityToEntropic $ Quantity $ ceiling f
       where
         f :: Rational
         f = (executionSteps e `times` priceExecutionSteps p) + (executionMemory e `times` priceExecutionMemory p)
         e = executionUnits
         p = executionUnitPrices
         times w c = fromIntegral w % 1 * c

    totalFee = baseFee +  fromIntegral numInputs * scriptFee
    (Quantity minValue) = entropicToQuantity $ fromIntegral numOutputs * minValuePerUTxO + totalFee
  -- this is not totally correct:
  -- beware of rounding errors !
    minValuePerInput = quantityToEntropic $ fromIntegral (if m==0 then d else d+1)
      where
        (d, m) = minValue `divMod` fromIntegral numInputs

--    fundSource :: FundSet.Target -> FundSet.FundSource
--    fundSource target = mkWalletFundSource walletRef $ FundSet.selectInputs ConfirmedBeforeReuse numInputs minTxValue PlainOldFund target

  fundSource <- liftIO (mkBufferedSource walletRef
                   (fromIntegral (unNumberOfTxs txCount) * numInputs)
                   minValuePerInput
                   (ZerepochScriptFund scriptFile scriptData) numInputs) >>= \case
    Right a  -> return a
    Left err -> throwE $ WalletError err

  let
    inToOut :: [Entropic] -> [Entropic]
    inToOut = FundSet.inputsToOutputsWithFee totalFee numOutputs
--    inToOut = FundSet.inputsToOutputsWithFee totalFee 1

    scriptWitness :: ScriptWitness WitCtxTxIn AurumEra
    scriptWitness = ZerepochScriptWitness
                          ZerepochScriptV1InAurum
                          ZerepochScriptV1
                          script
                          (ScriptDatumForTxIn scriptData)
                          scriptRedeemer
                          executionUnits

    txGenerator = genTxZerepochSpend protocolParameters collateral scriptWitness (mkFee totalFee) metadata

    fundToStore = mkWalletFundStore walletRef

    toUTxO :: FundSet.Target -> FundSet.SeqNumber -> ToUTxO AurumEra
    toUTxO target seqNumber = Wallet.mkUTxO networkId fundKey (InFlight target seqNumber)

    walletScript :: FundSet.Target -> WalletScript AurumEra
    walletScript = benchmarkWalletScript walletRef txGenerator txCount (const fundSource) inToOut toUTxO fundToStore

  case submitMode of
    NodeToNode -> do
      ret <- liftIO $ runExceptT $ GeneratorTx.walletBenchmark (btTxSubmit_ tracers) (btN2N_ tracers) connectClient
                               threadName targets tps LogErrors AsAurumEra txCount walletScript
      case ret of
        Left err -> liftTxGenError err
        Right ctl -> setName (ThreadName threadName) ctl
    _otherwise -> runWalletScriptInMode submitMode $ walletScript $ FundSet.Target "alternate-submit-mode"

dumpToFile :: FilePath -> TxInMode BccMode -> ActionM ()
dumpToFile filePath tx = liftIO $ dumpToFileIO filePath tx

dumpToFileIO :: FilePath -> TxInMode BccMode -> IO ()
dumpToFileIO filePath tx = appendFile filePath ('\n' : show tx)

-- Todo: make it possible to import several funds
-- (Split init and import)
importGenesisFund
   :: SubmitMode
   -> KeyName
   -> KeyName
   -> ActionM ()
importGenesisFund submitMode genesisKeyName destKey = do
  tracer <- btTxSubmit_ <$> get BenchTracers
  localSubmit <- case submitMode of
    LocalSocket -> getLocalSubmitTx
    NodeToNode -> throwE $ WalletError "NodeToNode mode not supported in importGenesisFund"
    DumpToFile filePath -> return $ \tx -> dumpToFileIO filePath tx >> return SubmitSuccess
    DiscardTX -> return $ \_ -> return SubmitSuccess
  networkId <- get NetworkId
  genesis  <- get Genesis
  fee      <- getUser TFee
  ttl      <- getUser TTTL
  fundKey  <- getName destKey
  genesisKey  <- getName genesisKeyName
  let
    coreCall :: forall era. IsSophieBasedEra era => AsType era -> ExceptT TxGenError IO Store.Fund
    coreCall _proxy = do
      let addr = Core.keyAddress @ era networkId fundKey
      f <- GeneratorTx.secureGenesisFund tracer localSubmit networkId genesis fee ttl genesisKey addr
      return (f, fundKey)
  liftCoreWithEra coreCall >>= \case
    Left err -> liftTxGenError err
    Right fund -> initGlobalWallet networkId fundKey fund

-- Todo split init and import of funds
initGlobalWallet :: NetworkId -> SigningKey PaymentKey -> Fund -> ActionM ()
initGlobalWallet networkId key ((txIn, outVal), skey) = do
  wallet <- liftIO $ initWallet networkId key
  liftIO (walletRefInsertFund wallet (FundSet.Fund $ mkFund outVal))
  set GlobalWallet wallet
 where
  mkFund = liftAnyEra $ \value -> FundInEra {
    _fundTxIn = txIn
  , _fundVal = value
  , _fundSigningKey = skey
  , _fundValidity = Confirmed
  , _fundVariant = PlainOldFund
  }

createChange :: SubmitMode -> PayMode -> Entropic -> Int -> ActionM ()
createChange submitMode payMode value count = case payMode of
  PayToAddr -> withEra $ createChangeInEra submitMode PlainOldFund value count
  -- Problem here: PayToCollateral will create an output marked as collateral
  -- and also return any change to a collateral, which makes the returned change unusable.
  PayToCollateral -> withEra $ createChangeInEra submitMode CollateralFund value count
  PayToScript scriptFile scriptData -> createChangeScriptFunds submitMode scriptFile scriptData value count

createChangeScriptFunds :: SubmitMode -> FilePath -> ScriptData -> Entropic -> Int -> ActionM ()
createChangeScriptFunds submitMode scriptFile scriptData value count = do
  walletRef <- get GlobalWallet
  networkId <- get NetworkId
  fundKey <- getName $ KeyName "pass-partout"
  fee <- getUser TFee  
  script <- liftIO $ ZerepochExample.readScript scriptFile --TODO: this should throw a file-not-found-error !
  let
    createCoins fundSource coins = do
      let
--        selector :: FundSet.FundSource
--        selector = mkWalletFundSource walletRef $ FundSet.selectMinValue $ sum coins + fee
        inOut :: [Entropic] -> [Entropic]
        inOut = Wallet.includeChange fee coins
        toUTxO = ZerepochExample.mkUtxoScript networkId fundKey (scriptFile, script, scriptData) Confirmed
        fundToStore = mkWalletFundStore walletRef

      tx <- liftIO $ sourceToStoreTransaction (genTx (mkFee fee) TxMetadataNone) fundSource inOut toUTxO fundToStore
      return $ fmap txInModeBcc tx
  createChangeGeneric submitMode createCoins value count

createChangeInEra :: forall era. IsSophieBasedEra era => SubmitMode -> Variant -> Entropic -> Int -> AsType era -> ActionM ()
createChangeInEra submitMode variant value count _proxy = do
  networkId <- get NetworkId
  fee <- getUser TFee
  walletRef <- get GlobalWallet
  fundKey <- getName $ KeyName "pass-partout"
  let
    createCoins :: FundSet.FundSource -> [Entropic] -> ActionM (Either String (TxInMode BccMode))
    createCoins fundSource coins = do
      let
--        selector :: FundSet.FundSource
--        selector = mkWalletFundSource walletRef $ FundSet.selectMinValue $ sum coins + fee
        inOut :: [Entropic] -> [Entropic]
        inOut = Wallet.includeChange fee coins
        toUTxO = Wallet.mkUTxOVariant variant networkId fundKey Confirmed
        fundToStore = mkWalletFundStore walletRef

      (tx :: Either String (Tx era)) <- liftIO $ sourceToStoreTransaction (genTx (mkFee fee) TxMetadataNone) fundSource inOut toUTxO fundToStore
      return $ fmap txInModeBcc tx
  createChangeGeneric submitMode createCoins value count

createChangeGeneric ::
     SubmitMode
  ->(FundSet.FundSource -> [Entropic] -> ActionM (Either String (TxInMode BccMode)))
  -> Entropic
  -> Int
  -> ActionM ()
createChangeGeneric submitMode createCoins value count = do
  submitTracer <- btTxSubmit_ <$> get BenchTracers
  fee <- getUser TFee
  walletRef <- get GlobalWallet
  let
    coinsList = replicate count value
    maxTxSize = 30
    chunks = chunkList maxTxSize coinsList
    txCount = length chunks
    txValue = fromIntegral (min maxTxSize count) * value + fee
    msg = mconcat [ "createChangeGeneric: outputs: ", show count
                  , " value: ", show value
                  , " number of txs: ", show txCount
                  ]
  liftIO $ traceWith submitTracer $ TraceBenchTxSubDebug msg
  fundSource <- liftIO (mkBufferedSource walletRef txCount txValue PlainOldFund 1) >>= \case
    Right a  -> return a
    Left err -> throwE $ WalletError err

  forM_ chunks $ \coins -> do
    gen <- createCoins fundSource coins
    case gen of
      Left err -> throwE $ WalletError err
      Right tx -> case submitMode of
        LocalSocket -> void $ localSubmitTx tx
        NodeToNode -> throwE $ WalletError "NodeToNode mode not supported in createChangeGeneric"
        DumpToFile filePath -> dumpToFile filePath tx
        DiscardTX -> return ()

  liftIO $ traceWith submitTracer $ TraceBenchTxSubDebug "createChangeGeneric: splitting done"
 where
  chunkList :: Int -> [a] -> [[a]]
  chunkList _ [] = []
  chunkList n xs = as : chunkList n bs where (as,bs) = splitAt n xs

{-
This is for dirty hacking and testing and quick-fixes.
Its a function that can be called from the JSON scripts
and for which the JSON encoding is "reserved".
-}
reserved :: [String] -> ActionM ()
reserved _ = do
  throwE $ UserError "no dirty hack is implemented"
