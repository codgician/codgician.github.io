module HakyllRecoverySpec (spec) where

import Compiler.HakyllRecovery
import Control.Exception (ErrorCall (..), throwIO)
import Control.Monad (when)
import Data.IORef
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = describe "Compiler.HakyllRecovery" $ do
  it "cleans Hakyll directories and retries once for known binary cache corruption" $
    withSystemTempDirectory "hakyll-recovery" $ \dir -> do
      let recovery =
            HakyllCacheRecovery
              { hakyllStoreDirectory = dir </> "_cache",
                hakyllDestinationDirectory = dir </> "_site",
                hakyllLegacyStoreDirectory = Just $ dir </> ".hakyll-cache"
              }
      createDirectoryIfMissing True $ hakyllStoreDirectory recovery
      createDirectoryIfMissing True $ hakyllDestinationDirectory recovery
      mapM_ (createDirectoryIfMissing True) (hakyllLegacyStoreDirectory recovery)
      attempts <- newIORef (0 :: Int)
      runWithHakyllCacheRecovery recovery $ do
        modifyIORef' attempts (+ 1)
        attempt <- readIORef attempts
        when (attempt == 1) $
          throwIO $
            ErrorCall "Data.Binary.Get.runGet at position 10: not enough bytes"
      readIORef attempts `shouldReturn` 2
      doesDirectoryExist (hakyllStoreDirectory recovery) `shouldReturn` False
      doesDirectoryExist (hakyllDestinationDirectory recovery) `shouldReturn` False

  it "does not clean project artifact cache" $
    withSystemTempDirectory "hakyll-recovery" $ \dir -> do
      let artifactDir = dir </> "_artifacts"
          recovery =
            HakyllCacheRecovery
              { hakyllStoreDirectory = dir </> "_cache",
                hakyllDestinationDirectory = dir </> "_site",
                hakyllLegacyStoreDirectory = Just $ dir </> ".hakyll-cache"
              }
      createDirectoryIfMissing True artifactDir
      attempts <- newIORef (0 :: Int)
      runWithHakyllCacheRecovery recovery $ do
        modifyIORef' attempts (+ 1)
        attempt <- readIORef attempts
        when (attempt == 1) $
          throwIO $
            ErrorCall "Data.Binary.Get.runGet at position 10: too few bytes"
      doesDirectoryExist artifactDir `shouldReturn` True

  it "propagates unknown exceptions without retrying" $ do
    attempts <- newIORef (0 :: Int)
    let recovery =
          HakyllCacheRecovery
            { hakyllStoreDirectory = "_cache",
              hakyllDestinationDirectory = "_site",
              hakyllLegacyStoreDirectory = Just ".hakyll-cache"
            }
    runWithHakyllCacheRecovery recovery (modifyIORef' attempts (+ 1) >> throwIO (ErrorCall "different failure"))
      `shouldThrow` errorCall "different failure"
    readIORef attempts `shouldReturn` 1
