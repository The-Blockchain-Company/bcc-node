{-# LANGUAGE CPP #-}

module Bcc.Logging.Resources
    ( Resources(..)
    , ResourceStats
    , readResourceStats
    ) where


import           Bcc.Logging.Resources.Types
#if defined(linux_HOST_OS)
import qualified Bcc.Logging.Resources.Linux as Platform
#elif defined(mingw32_HOST_OS)
import qualified Bcc.Logging.Resources.Windows as Platform
#elif defined(darwin_HOST_OS)
import qualified Bcc.Logging.Resources.Darwin as Platform
#else
import qualified Bcc.Logging.Resources.Dummy as Platform
#endif


readResourceStats :: IO (Maybe ResourceStats)
readResourceStats = Platform.readRessoureStatsInternal
