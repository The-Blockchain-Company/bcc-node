{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import           Prelude

import           Hedgehog (Property, property, success)
import           Hedgehog.Extras.Stock.OS (isWin32)
import qualified System.Environment as E
import           Test.Tasty (TestTree)
import qualified Test.Tasty as T
import qualified Test.Tasty.Hedgehog as H
import qualified Test.Tasty.Ingredients as T

import qualified Spec.Zerepoch.Direct.ScriptContextEquality
import qualified Spec.Zerepoch.Direct.ScriptContextEqualityMint
import qualified Spec.Zerepoch.Direct.TxInLockingZerepoch
import qualified Spec.Zerepoch.Script.TxInLockingZerepoch
import qualified Spec.Zerepoch.SubmitApi.TxInLockingZerepoch

tests :: IO TestTree
tests = do
  pure $ T.testGroup "test/Spec.hs"
    [ T.testGroup "Spec"
      [ H.testProperty "Spec.Zerepoch.Direct.TxInLockingZerepoch" Spec.Zerepoch.Direct.TxInLockingZerepoch.hprop_zerepoch
      , H.testProperty "Spec.Zerepoch.Script.TxInLockingZerepoch" Spec.Zerepoch.Script.TxInLockingZerepoch.hprop_zerepoch
      , H.testProperty "Spec.Zerepoch.SubmitApi.TxInLockingZerepoch" Spec.Zerepoch.SubmitApi.TxInLockingZerepoch.hprop_zerepoch
      , ignoreOnWindows "Spec.Zerepoch.Direct.ScriptContextEquality"  Spec.Zerepoch.Direct.ScriptContextEquality.hprop_zerepoch_script_context_equality
      , ignoreOnWindows "Spec.Zerepoch.Direct.ScriptContextEqualityMint" Spec.Zerepoch.Direct.ScriptContextEqualityMint.hprop_zerepoch_script_context_mint_equality
      ]
    ]

ignoreOnWindows :: String -> Property -> TestTree
ignoreOnWindows pName prop =
  if isWin32
  then H.testProperty ("Property not tested on Windows: " ++ pName) $ property success
  else H.testProperty pName prop


ingredients :: [T.Ingredient]
ingredients = T.defaultIngredients

main :: IO ()
main = do
  args <- E.getArgs

  E.withArgs args $ tests >>= T.defaultMainWithIngredients ingredients
