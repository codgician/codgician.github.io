module Compiler.HakyllRecovery
  ( HakyllCacheRecovery (..),
    runWithHakyllCacheRecovery,
  )
where

import Control.Exception (AsyncException, SomeException, catch, displayException, fromException, throwIO)
import Control.Monad (forM_, when)
import Data.List (isInfixOf)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import System.IO (hPutStrLn, stderr)

data HakyllCacheRecovery = HakyllCacheRecovery
  { hakyllStoreDirectory :: FilePath,
    hakyllDestinationDirectory :: FilePath,
    hakyllLegacyStoreDirectory :: Maybe FilePath
  }
  deriving (Eq, Show)

runWithHakyllCacheRecovery :: HakyllCacheRecovery -> IO () -> IO ()
runWithHakyllCacheRecovery recovery action =
  action `catch` handleFirstFailure
  where
    handleFirstFailure e
      | Just async <- fromException e = throwIO (async :: AsyncException)
      | isHakyllCacheCorruption $ displayException e = do
          hPutStrLn stderr "Hakyll cache corruption detected, cleaning and retrying once."
          cleanHakyllCache recovery
          action
      | otherwise = throwIO (e :: SomeException)

isHakyllCacheCorruption :: String -> Bool
isHakyllCacheCorruption msg =
  "Data.Binary.Get.runGet" `isInfixOf` msg
    || "not enough bytes" `isInfixOf` msg
    || "too few bytes" `isInfixOf` msg

cleanHakyllCache :: HakyllCacheRecovery -> IO ()
cleanHakyllCache recovery =
  forM_
    ( [ hakyllStoreDirectory recovery,
        hakyllDestinationDirectory recovery
      ]
        <> maybe [] pure (hakyllLegacyStoreDirectory recovery)
    )
    removeIfExists

removeIfExists :: FilePath -> IO ()
removeIfExists dir = do
  exists <- doesDirectoryExist dir
  when exists $ removeDirectoryRecursive dir
