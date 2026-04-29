module Main (main) where

import Compiler.HakyllRecovery (HakyllCacheRecovery (..), runWithHakyllCacheRecovery)
import Site (hakyllMain)

main :: IO ()
main = runWithHakyllCacheRecovery recovery hakyllMain

recovery :: HakyllCacheRecovery
recovery =
  HakyllCacheRecovery
    { hakyllStoreDirectory = "_cache",
      hakyllDestinationDirectory = "_site",
      hakyllLegacyStoreDirectory = Just ".hakyll-cache"
    }
