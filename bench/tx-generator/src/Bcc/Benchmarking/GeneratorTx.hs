{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Bcc.Benchmarking.GeneratorTx
  ( AsyncBenchmarkControl
  , NumberOfTxs(..)
  , NumberOfInputsPerTx(..)
  , NumberOfOutputsPerTx(..)
  , InitCooldown(..)
  , TPSRate(..)
  , TxAdditionalSize(..)
  , TxGenError
  , walletBenchmark
  , asyncBenchmark
  , readSigningKey
  , runBenchmark
  , secureGenesisFund
  , splitFunds
  , txGenerator
  , waitBenchmark
  ) where

import           Bcc.Prelude
import           Prelude (String, error, id)

import qualified Control.Concurrent.STM as STM
import           Control.Monad (fail)
import           Control.Monad.Trans.Except.Extra (left, newExceptT, right)
import           Control.Tracer (Tracer, traceWith)
import qualified Data.Time.Clock as Clock

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.Text (pack)
import           Network.Socket (AddrInfo (..), AddrInfoFlag (..), Family (..), SocketType (Stream),
                   addrFamily, addrFlags, addrSocketType, defaultHints, getAddrInfo)

import           Bcc.CLI.Types (SigningKeyFile (..))
import           Bcc.Node.Types

import           Shardagnostic.Consensus.Sophie.Eras (StandardSophie)

import           Bcc.Api hiding (txFee)

import qualified Bcc.Benchmarking.FundSet as FundSet
import           Bcc.Benchmarking.GeneratorTx.Error
import           Bcc.Benchmarking.GeneratorTx.Genesis
import           Bcc.Benchmarking.GeneratorTx.NodeToNode
import           Bcc.Benchmarking.GeneratorTx.SizedMetadata (mkMetadata)
import           Bcc.Benchmarking.GeneratorTx.Submission
import           Bcc.Benchmarking.GeneratorTx.SubmissionClient
import           Bcc.Benchmarking.GeneratorTx.Tx
import           Bcc.Benchmarking.Tracer
import           Bcc.Benchmarking.Types
import           Bcc.Benchmarking.Wallet

import           Bcc.Ledger.Sophie.API (SophieGenesis)
import           Shardagnostic.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))

readSigningKey :: SigningKeyFile -> ExceptT TxGenError IO (SigningKey PaymentKey)
readSigningKey =
  withExceptT TxFileError . newExceptT . readKey . unSigningKeyFile
 where
  readKey :: FilePath -> IO (Either (FileError TextEnvelopeError) (SigningKey PaymentKey))
  readKey f = flip readFileTextEnvelopeAnyOf f
    [ FromSomeType (AsSigningKey AsGenesisUTxOKey) castSigningKey
    , FromSomeType (AsSigningKey AsPaymentKey) id
    ]

secureGenesisFund :: forall era. IsSophieBasedEra era
  => Tracer IO (TraceBenchTxSubmit TxId)
  -> (TxInMode BccMode -> IO (SubmitResult (TxValidationErrorInMode BccMode)))
  -> NetworkId
  -> SophieGenesis StandardSophie
  -> Entropic
  -> SlotNo
  -> SigningKey PaymentKey
  -> AddressInEra era
  -> ExceptT TxGenError IO Fund
secureGenesisFund submitTracer localSubmitTx networkId genesis txFee ttl key outAddr = do
  let (_inAddr, entropic) = genesisFundForKey @ era networkId genesis key
      (tx, fund) =
         genesisExpenditure networkId key outAddr entropic txFee ttl
  r <- liftIO $
    catches (localSubmitTx $ txInModeBcc tx)
      [ Handler $ \e@SomeException{} ->
          fail $ mconcat
            [ "Exception while moving genesis funds via local socket: "
            , show e
            ]]
  case r of
    SubmitSuccess ->
      liftIO . traceWith submitTracer . TraceBenchTxSubDebug
      $ mconcat
      [ "******* Funding secured ("
      , show $ fundTxIn fund, " -> ", show $ fundDafiValue fund
      , ")"]
    SubmitFail e -> fail $ show e
  return fund

-----------------------------------------------------------------------------------------
-- Obtain initial funds.
-----------------------------------------------------------------------------------------
splitFunds  :: forall era. IsSophieBasedEra era
  => Tracer IO (TraceBenchTxSubmit TxId)
  -> (TxInMode BccMode -> IO (SubmitResult (TxValidationErrorInMode BccMode)))
  -> Entropic
  -> NumberOfTxs
  -> NumberOfInputsPerTx
  -> SigningKey PaymentKey
  -> AddressInEra era
  -> Fund
  -> ExceptT TxGenError IO [Fund]
splitFunds
    submitTracer
    localSubmitTx
    fee
    (NumberOfTxs numTxs)
    (NumberOfInputsPerTx txFanin)
    sourceKey
    globalOutAddr
    fundsTxIO = do
  let -- The number of splitting txout entries (corresponds to the number of all inputs we will need).
      (Quantity rawCoin) = entropicToQuantity $ fundDafiValue fundsTxIO
      (Quantity feeRaw) = entropicToQuantity fee
      numRequiredTxOuts = numTxs * fromIntegral txFanin
      splitFanout = 60 :: Word64 -- near the upper bound so as not to exceed the tx size limit
      (nFullTxs, remainder) = numRequiredTxOuts `divMod` splitFanout
      numSplitTxs = nFullTxs + if remainder > 0 then 1 else 0

  let -- Split the funds to 'numRequiredTxOuts' equal parts, subtracting the possible fees.
      -- a safe number for fees is numRequiredTxOuts' * feePerTx.
      outputSliceWithFees = rawCoin `div` fromIntegral numRequiredTxOuts
      outputSlice = outputSliceWithFees - feeRaw
      splitValue = mkTxOutValueDafiOnly $ quantityToEntropic $ Quantity outputSlice
      -- The same output for all splitting transaction: send the same 'splitValue'
      -- to the same 'sourceAddress'.
      -- Create and sign splitting txs.
      splittingTxs  = createSplittingTxs sourceKey
                                         fundsTxIO
                                         numRequiredTxOuts
                                         splitFanout
                                         42
                                         splitValue
                                         []
  -- Submit all splitting transactions sequentially.
  liftIO $ traceWith submitTracer $ TraceBenchTxSubDebug $ mconcat
     [ "Coin splitting (values are Entropics): "
     , "total funds: ", show rawCoin, ", "
     , "txouts needed: ", show numRequiredTxOuts, ", "
     , "txout slice with fees: ", show outputSliceWithFees, ", "
     , "fees: ", show feeRaw, ", "
     , "txout slice: ", show outputSlice, ", "
     , "splitting fanout: ", show splitFanout, ", "
     , "splitting tx count: ", show (length splittingTxs)
     ]
  forM_ (zip splittingTxs [0::Int ..]) $ \((tx, _), i) ->
    liftIO (localSubmitTx $ txInModeBcc tx)
    >>= \case
      SubmitSuccess -> pure ()
      SubmitFail x -> left . SplittingSubmissionError $ mconcat
           ["Coin splitting submission failed (", show i :: Text
           , "/", show numSplitTxs :: Text
           , "): ", show x :: Text
           , "\n  Tx: ", show tx]
  liftIO $ putStrLn ("submitted all coin splitting Txs." :: Text)

  -- Re-create availableFunds with information about all splitting transactions
  -- (it will be used for main transactions).
  right $ concatMap snd splittingTxs
 where
  -- create txs which split the funds to numTxOuts equal parts
  createSplittingTxs
    :: SigningKey PaymentKey
    -> Fund
    -> Word64
    -> Word64
    -> Int
    -> TxOutValue era
    -> [(Tx era, [Fund])]
    -> [(Tx era, [Fund])]
  createSplittingTxs sKey initialFund numTxOuts maxOutsPerInitTx identityIndex txOut acc
    | numTxOuts <= 0 = reverse acc
    | otherwise =
        let numOutsPerInitTx = min maxOutsPerInitTx numTxOuts
            -- same TxOut for all
            outs = zip [identityIndex ..
                        identityIndex + fromIntegral numOutsPerInitTx - 1]
                       (repeat (TxOut globalOutAddr txOut TxOutDatumNone))
            (mFunds, _fees, outIndices, splitTx) =
              mkTransactionGen sKey (initialFund :| []) globalOutAddr outs TxMetadataNone fee
            !splitTxId = getTxId $ getTxBody splitTx
            txIOList = flip map (Map.toList outIndices) $
                \(_, txInIndex) ->
                  let !txIn  = TxIn splitTxId txInIndex
                  in mkFund txIn txOut
        in
          case mFunds of
            Nothing                 -> reverse $ (splitTx, txIOList) : acc
            Just (txInIndex, val) ->
              let !txInChange  = TxIn splitTxId txInIndex
                  !txChangeValue = mkTxOutValueDafiOnly @ era val
              in
                -- from the change create the next tx with numOutsPerInitTx UTxO entries
                createSplittingTxs sKey
                                   (mkFund txInChange txChangeValue)
                                   (numTxOuts - numOutsPerInitTx)
                                   numOutsPerInitTx
                                   (identityIndex + fromIntegral numOutsPerInitTx)
                                   txOut
                                   ((splitTx, txIOList) : acc)

-----------------------------------------------------------------------------------------
-- | Run benchmark using top level tracers..
-----------------------------------------------------------------------------------------

-- This is the entry point for the CLI args tx-generator
-- {-# DEPRECATED runBenchmark "to be removed: use asyncBenchmark instead" #-}
runBenchmark :: forall era. IsSophieBasedEra era
  => Tracer IO (TraceBenchTxSubmit TxId)
  -> Tracer IO NodeToNodeSubmissionTrace
  -> ConnectClient
  -> NonEmpty NodeIPv4Address
  -> TPSRate
  -> SubmissionErrorPolicy
  -> [Tx era]
  -> ExceptT TxGenError IO ()
runBenchmark
  traceSubmit
  traceN2N
  connectClient
  targets
  tpsRate
  errorPolicy
  finalTransactions
  = do
    ctl <- asyncBenchmark
                       traceSubmit
                       traceN2N
                       connectClient
                       "UnknownThreadLabel"
                       targets
                       tpsRate
                       errorPolicy
                       finalTransactions
    waitBenchmark traceSubmit ctl

type AsyncBenchmarkControl = (Async (), [Async ()], IO SubmissionSumjen, IO ())

waitBenchmark :: Tracer IO (TraceBenchTxSubmit TxId) -> AsyncBenchmarkControl -> ExceptT TxGenError IO ()
waitBenchmark traceSubmit (feeder, workers, mkSumjen, _) = liftIO $ do
  mapM_ waitCatch (feeder : workers)
  traceWith traceSubmit =<< TraceBenchTxSubSumjen <$> mkSumjen

asyncBenchmark :: forall era. IsSophieBasedEra era
  => Tracer IO (TraceBenchTxSubmit TxId)
  -> Tracer IO NodeToNodeSubmissionTrace
  -> ConnectClient
  -> String
  -> NonEmpty NodeIPv4Address
  -> TPSRate
  -> SubmissionErrorPolicy
  -> [Tx era]
  -> ExceptT TxGenError IO AsyncBenchmarkControl
asyncBenchmark
  traceSubmit
  traceN2N
  connectClient
  threadName
  targets
  tpsRate
  errorPolicy
  transactions
  = liftIO $ do
  traceDebug "******* Tx generator, phase 2: pay to recipients *******"

  remoteAddresses <- forM targets lookupNodeAddress
  let numTargets :: Natural = fromIntegral $ NE.length targets

  traceDebug $ "******* Tx generator, launching Tx peers:  " ++ show (NE.length remoteAddresses) ++ " of them"

  startTime <- Clock.getCurrentTime
  txSendQueue <- STM.newTBQueueIO 32

  reportRefs <- STM.atomically $ replicateM (fromIntegral numTargets) STM.newEmptyTMVar

  allAsyncs <- forM (zip reportRefs $ NE.toList remoteAddresses) $
    \(reportRef, remoteAddr) -> do
      let errorHandler = handleTxSubmissionClientError traceSubmit remoteAddr reportRef errorPolicy
          client = txSubmissionClient
                     traceN2N
                     traceSubmit
                     (legacyTxSource txSendQueue)
                     (submitSubmissionThreadStats reportRef)
      async $ handle errorHandler (connectClient remoteAddr client)

  tpsFeeder <- async $ tpsLimitedTxFeeder traceSubmit numTargets txSendQueue tpsRate transactions

  let tpsFeederShutdown = do
        cancel tpsFeeder
        liftIO $ tpsLimitedTxFeederShutdown numTargets txSendQueue

  return (tpsFeeder, allAsyncs, mkSubmissionSumjen threadName startTime reportRefs, tpsFeederShutdown)
 where
  traceDebug :: String -> IO ()
  traceDebug =   traceWith traceSubmit . TraceBenchTxSubDebug

-- | At this moment 'sourceAddress' contains a huge amount of money (lets call it A).
--   Now we have to split this amount to N equal parts, as a result we'll have
--   N UTxO entries, and alltogether these entries will contain the same amount A.
--   E.g. (1 entry * 1000 DAFI) -> (10 entries * 100 DAFI).
--   Technically all splitting transactions will send money back to 'sourceAddress'.

lookupNodeAddress ::
  NodeAddress' NodeHostIPv4Address -> IO AddrInfo
lookupNodeAddress node = do
  (remoteAddr:_) <- getAddrInfo (Just hints) (Just targetNodeHost) (Just targetNodePort)
  return remoteAddr
 where
  targetNodeHost = show . unNodeHostIPv4Address $ naHostAddress node
  targetNodePort = show $ naPort node
  hints :: AddrInfo
  hints = defaultHints
    { addrFlags      = [AI_PASSIVE]
    , addrFamily     = AF_INET
    , addrSocketType = Stream
    , addrCanonName  = Nothing
    }



-----------------------------------------------------------------------------------------
-- | Work with tx generator thread (for Phase 2).
-----------------------------------------------------------------------------------------
txGenerator
  :: forall era
  .  IsSophieBasedEra era
  => Tracer IO (TraceBenchTxSubmit TxId)
  -> Entropic
  -> NumberOfTxs
  -> NumberOfInputsPerTx
  -> NumberOfOutputsPerTx
  -> TxAdditionalSize
  -> AddressInEra era
  -> SigningKey PaymentKey
  -> Int
  -> [Fund]
  -> ExceptT TxGenError IO [Tx era]
txGenerator
  tracer
  txFee
  (NumberOfTxs numOfTransactions)
  (NumberOfInputsPerTx numOfInsPerTx)
  (NumberOfOutputsPerTx numOfOutsPerTx)
  (TxAdditionalSize txAdditionalSize)
  recipientAddress
  sourceKey
  numOfTargetNodes
  fundsWithSufficientCoins
  = do
  liftIO . traceWith tracer . TraceBenchTxSubDebug
    $ " Generating " ++ show numOfTransactions
      ++ " transactions, for " ++ show numOfTargetNodes
      ++ " peers, fee " ++ show txFee
      ++ ", value " ++ show valueForRecipient
      ++ ", totalValue " ++ show totalValue
  metadata <- case mkMetadata txAdditionalSize of
    Right m -> return m
    Left err -> throwE $ BadPayloadSize $ pack err
  txs <- createMainTxs numOfTransactions numOfInsPerTx metadata fundsWithSufficientCoins
  liftIO . traceWith tracer . TraceBenchTxSubDebug
    $ " Done, " ++ show numOfTransactions ++ " were generated."
  pure txs
 where
  -- Num of recipients is equal to 'numOuts', so we think of
  -- recipients as the people we're going to pay to.
  recipients = zip [initRecipientIndex .. initRecipientIndex + numOfOutsPerTx - 1]
                   (repeat txOut)
  initRecipientIndex = 0 :: Int
  -- The same output for all transactions.
  valueForRecipient = quantityToEntropic $ Quantity 1000000 -- 10 DAFI
  !txOut = TxOut recipientAddress (mkTxOutValueDafiOnly valueForRecipient) TxOutDatumNone
  totalValue = valueForRecipient + txFee
  -- Send possible change to the same 'recipientAddress'.
  addressForChange = recipientAddress

  -- Create all main transactions, using available funds.
  createMainTxs
    :: Word64
    -> Int
    -> TxMetadataInEra era
    -> [Fund]
    -> ExceptT TxGenError IO [Tx era]
  createMainTxs 0 _ _ _= right []
  createMainTxs txsNum insNumPerTx metadata funds = do
    (txInputs, updatedFunds) <- getTxInputs insNumPerTx funds
    let (_, _, _, txAux :: Tx era) =
          mkTransactionGen
            sourceKey
            (NE.fromList txInputs)
            addressForChange
            recipients
            metadata
            txFee
    (txAux :) <$> createMainTxs (txsNum - 1) insNumPerTx metadata updatedFunds

  -- Get inputs for one main transaction, using available funds.
  getTxInputs
    :: Int
    -> [Fund]
    -> ExceptT TxGenError IO ( [Fund] , [Fund])
  getTxInputs 0 funds = right ([], funds)
  getTxInputs insNumPerTx funds = do
    (found, updatedFunds) <- findAvailableFunds funds totalValue
    (inputs, updatedFunds') <- getTxInputs (insNumPerTx - 1) updatedFunds
    right (found : inputs, updatedFunds')

  -- Find a source of available funds, removing it from the availableFunds
  -- for preventing of double spending.
  findAvailableFunds
    :: [Fund]     -- funds we are trying to find in
    -> Entropic                -- with at least this associated value
    -> ExceptT TxGenError IO (Fund, [Fund])
  findAvailableFunds funds thresh =
    case break (predTxD thresh) funds of
      (_, [])    ->
        left $ InsufficientFundsForRecipientTx
                 thresh
                 (maximum $ map fundDafiValue funds)
      (toofews, found:rest) -> right (found, toofews <> rest)

  -- Find the first tx output that contains sufficient amount of money.
  predTxD :: Entropic -> Fund -> Bool
  predTxD valueThreshold f = fundDafiValue f >= valueThreshold

---------------------------------------------------------------------------------------------------
-- Txs for submission.
---------------------------------------------------------------------------------------------------

handleTxSubmissionClientError ::
     Tracer IO (TraceBenchTxSubmit TxId)
  -> Network.Socket.AddrInfo
  -> ReportRef
  -> SubmissionErrorPolicy
  -> SomeException
  -> IO ()
handleTxSubmissionClientError
  traceSubmit
  remoteAddr
  reportRef
  errorPolicy
  (SomeException err) = do
    submitThreadReport reportRef (Left errDesc)
    case errorPolicy of
      FailOnError -> throwIO err
      LogErrors   -> traceWith traceSubmit $
        TraceBenchTxSubError (pack errDesc)
   where
    errDesc = mconcat
      [ "Exception while talking to peer "
      , " (", show (addrAddress remoteAddr), "): "
      , show err]

walletBenchmark :: forall era. IsSophieBasedEra era
  => Tracer IO (TraceBenchTxSubmit TxId)
  -> Tracer IO NodeToNodeSubmissionTrace
  -> ConnectClient
  -> String
  -> NonEmpty NodeIPv4Address
  -> TPSRate
  -> SubmissionErrorPolicy
  -> AsType era
  -> NumberOfTxs
  -> (FundSet.Target -> WalletScript era)
  -> ExceptT TxGenError IO AsyncBenchmarkControl
walletBenchmark
  traceSubmit
  traceN2N
  connectClient
  threadName
  targets
  tpsRate
  errorPolicy
  _era
  count
  walletScript
  = liftIO $ do
  traceDebug "******* Tx generator, phase 2: pay to recipients *******"

  remoteAddresses <- forM targets lookupNodeAddress
  let numTargets :: Natural = fromIntegral $ NE.length targets

  traceDebug $ "******* Tx generator, launching Tx peers:  " ++ show (NE.length remoteAddresses) ++ " of them"

  startTime <- Clock.getCurrentTime
  txSendQueue <- STM.newTBQueueIO 32

  reportRefs <- STM.atomically $ replicateM (fromIntegral numTargets) STM.newEmptyTMVar

  allAsyncs <- forM (zip reportRefs $ NE.toList remoteAddresses) $
    \(reportRef, remoteAddr) -> do
      let errorHandler = handleTxSubmissionClientError traceSubmit remoteAddr reportRef errorPolicy
          client = txSubmissionClient
                     traceN2N
                     traceSubmit
                     (walletTxSource (walletScript (FundSet.Target $ show remoteAddr)) txSendQueue)
                     (submitSubmissionThreadStats reportRef)
      async $ handle errorHandler (connectClient remoteAddr client)

  tpsFeeder <- async $ tpsLimitedTxFeeder traceSubmit numTargets txSendQueue tpsRate
                        $ replicate (fromIntegral $ unNumberOfTxs count) (error "dummy transaction" :: Tx era)

  let tpsFeederShutdown = do
        cancel tpsFeeder
        liftIO $ tpsLimitedTxFeederShutdown numTargets txSendQueue

  return (tpsFeeder, allAsyncs, mkSubmissionSumjen threadName startTime reportRefs, tpsFeederShutdown)
 where
  traceDebug :: String -> IO ()
  traceDebug =   traceWith traceSubmit . TraceBenchTxSubDebug
