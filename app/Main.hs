module Main (main) where

import Control.Exception (SomeException, catch, displayException, throwIO)
import Control.Monad (when)
import Data.List (isInfixOf)
import Site (hakyllMain)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = hakyllMain `catch` handleCacheCorruption

handleCacheCorruption :: SomeException -> IO ()
handleCacheCorruption e
  | isCacheCorruption (displayException e) = do
      hPutStrLn stderr "Cache corruption detected, cleaning and retrying..."
      cleanCache
      hakyllMain
  | otherwise = throwIO e

isCacheCorruption :: String -> Bool
isCacheCorruption msg =
  "Data.Binary.Get.runGet" `isInfixOf` msg
    || "not enough bytes" `isInfixOf` msg
    || "too few bytes" `isInfixOf` msg

cleanCache :: IO ()
cleanCache = do
  removeIfExists "_cache"
  removeIfExists "_site"
  removeIfExists ".hakyll-cache"

removeIfExists :: FilePath -> IO ()
removeIfExists dir = do
  exists <- doesDirectoryExist dir
  when exists $ removeDirectoryRecursive dir
