{-# LANGUAGE FlexibleContexts #-}
module Examples.Documentation (
  docTracers
) where

import qualified Data.Text.IO as T

import           Bcc.Logging
import           Examples.TestObjects

docTracers :: IO ()
docTracers = do
  t <- standardTracer
  t1' <- humanFormatter True "bcc" t
  let t1 = withSeverityTraceForgeEvent
                (appendName "node" t1')
  t2' <- machineFormatter "bcc" t
  let t2 = withSeverityTraceForgeEvent
                (appendName "node" t2')
  bl <- documentMarkdown traceForgeEventDocu [t1, t2]
  res <- buildersToText bl emptyTraceConfig
  T.writeFile "/home/yupanqui/BCIO/Testdocu.md" res
