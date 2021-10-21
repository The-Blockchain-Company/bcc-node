{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.TxView (txViewTests) where

import           Bcc.Prelude

import           Hedgehog (Group (..), Property, checkSequential)
import           Hedgehog.Extras.Test.Base (moduleWorkspace, propertyOnce)

import           Test.OptParse (execBccCLI, noteTempFile)
import           Test.Utilities (diffVsGoldenFile)

{- HLINT ignore "Use camelCase" -}

txViewTests :: IO Bool
txViewTests =
  checkSequential $
    Group "`transaction view` Goldens"
      [ ("golden_view_cole",   golden_view_cole)
      , ("golden_view_sophie", golden_view_sophie)
      , ("golden_view_evie", golden_view_evie)
      , ("golden_view_jen",    golden_view_jen)
      -- , ("golden_view_aurum",  golden_view_aurum)
      ]

golden_view_cole :: Property
golden_view_cole =
  propertyOnce $
  moduleWorkspace "tmp" $ \tempDir -> do
    transactionBodyFile <- noteTempFile tempDir "transaction-body-file"

    -- Create transaction body
    void $
      execBccCLI
        [ "transaction", "build-raw"
        , "--cole-era"
        , "--tx-in"
        ,   "F8EC302D19E3C8251C30B1434349BF2E949A1DBF14A4EBC3D512918D2D4D5C56\
            \#88"
        , "--tx-out"
        ,   "5oP9ib6ym3XfwXuy3ksXZzgtBzXSArXAACQVXKqcPhiLnHVYjXJNu2T6Zomh8LAWLV\
            \+68"
        , "--out-file", transactionBodyFile
        ]

    -- View transaction body
    result <-
      execBccCLI
        ["transaction", "view", "--tx-body-file", transactionBodyFile]
    diffVsGoldenFile result "test/data/golden/cole/transaction-view.out"

golden_view_sophie :: Property
golden_view_sophie =
  propertyOnce $
  moduleWorkspace "tmp" $ \tempDir -> do
    transactionBodyFile <- noteTempFile tempDir "transaction-body-file"

    -- Create transaction body
    void $
      execBccCLI
        [ "transaction", "build-raw"
        , "--sophie-era"
        , "--tx-in"
        ,   "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891\
            \#29"
        , "--tx-out"
        ,   "addr_test1vz7w0r9epak6nmnh3mc8e2ypkjyu8zsc3xf7dpct6k577acxmcfyv+31"
        , "--fee", "32"
        , "--invalid-hereafter", "33"
        , "--withdrawal"
        ,   "stake_test1up00fz9lyqs5sjks82k22eqz7a9srym9vysjgp3h2ua2v2cm522kg\
            \+42"
        , "--out-file", transactionBodyFile
        ]

    -- View transaction body
    result <-
      execBccCLI
        ["transaction", "view", "--tx-body-file", transactionBodyFile]
    diffVsGoldenFile result "test/data/golden/sophie/transaction-view.out"

golden_view_evie :: Property
golden_view_evie =
  propertyOnce $
  moduleWorkspace "tmp" $ \tempDir -> do
    transactionBodyFile <- noteTempFile tempDir "transaction-body-file"

    -- Create transaction body
    void $
      execBccCLI
        [ "transaction", "build-raw"
        , "--evie-era"
        , "--tx-in"
        ,   "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891\
            \#94"
        , "--tx-out"
        ,   "addr_test1qrefnr4k09pvge6dq83v6s67ruter8sftmky8qrmkqqsxy7q5psgn8tgqmupq4r79jmxlyk4eqt6z6hj5g8jd8393msqaw47f4\
            \+99"
        , "--fee", "100"
        , "--invalid-hereafter", "101"
        , "--out-file", transactionBodyFile
        ]

    -- View transaction body
    result <-
      execBccCLI
        ["transaction", "view", "--tx-body-file", transactionBodyFile]
    diffVsGoldenFile result "test/data/golden/evie/transaction-view.out"

golden_view_jen :: Property
golden_view_jen =
  propertyOnce $
  moduleWorkspace "tmp" $ \tempDir -> do
    transactionBodyFile <- noteTempFile tempDir "transaction-body-file"

    -- Create transaction body
    void $
      execBccCLI
        [ "transaction", "build-raw"
        , "--jen-era"
        , "--tx-in"
        ,   "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891\
            \#135"
        , "--tx-out"
        ,   "addr_test1qrefnr4k09pvge6dq83v6s67ruter8sftmky8qrmkqqsxy7q5psgn8tgqmupq4r79jmxlyk4eqt6z6hj5g8jd8393msqaw47f4\
            \+138"
        , "--fee", "139"
        , "--invalid-before", "140"
        , "--mint"
        ,   "42 d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf\
            \ + \
            \43 52dc3d43b6d2465e96109ce75ab61abe5e9c1d8a3c9ce6ff8a3af528.snow\
            \ + \
            \44 d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf.sky"
        , "--minting-script-file", "test/data/golden/jen/scripts/mint.all"
        , "--minting-script-file", "test/data/golden/jen/scripts/mint.any"
        , "--out-file", transactionBodyFile
        ]

    -- View transaction body
    result <-
      execBccCLI
        ["transaction", "view", "--tx-body-file", transactionBodyFile]
    diffVsGoldenFile result "test/data/golden/jen/transaction-view.out"
