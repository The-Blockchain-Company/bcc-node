{-# LANGUAGE LambdaCase #-}
module Bcc.Benchmarking.ListBufferedSelector
(
  mkBufferedSource
)
where
import           Prelude

import           Control.Concurrent.MVar
import           Bcc.Api

import           Bcc.Benchmarking.FundSet as FundSet
import           Bcc.Benchmarking.Wallet as Wallet

mkBufferedSource ::
     WalletRef
  -> Int
  -> Entropic
  -> Variant
  -> Int
  -> IO (Either String FundSource)
mkBufferedSource walletRef count minValue variant munch
  = mkWalletFundSource walletRef (selectToBuffer count minValue variant) >>= \case
    Left err -> return $ Left err
    Right funds -> do
      buffer <- newMVar funds
      return $ Right $ listSource buffer munch

listSource :: MVar [Fund] -> Int -> IO (Either String [Fund])
listSource mvar count = modifyMVarMasked mvar popFunds
  where
    popFunds funds
      = if length sel == count
           then return (rest, Right sel)
           else return (funds, Left "ListSource: out of funds")
      where (sel, rest) = splitAt count funds
