{-# LANGUAGE OverloadedStrings #-}

module Compiler.Cache
  ( cachedRender,
    CacheConfig (..),
  )
where

import Control.Exception (SomeException, catch)
import Crypto.Hash.SHA256 (hash)
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath ((<.>), (</>))
import System.IO (hClose)
import System.IO.Temp (openTempFile)

data CacheConfig = CacheConfig
  { cacheDir :: FilePath,
    toolName :: Text,
    toolVersion :: Text,
    toolOptions :: Text,
    filterVersion :: Text
  }

buildCacheKey :: CacheConfig -> Text -> Text
buildCacheKey cfg content =
  let components =
        T.intercalate "\n"
          [ "v1",
            toolName cfg,
            toolVersion cfg,
            toolOptions cfg,
            filterVersion cfg,
            content
          ]
      hashBytes = hash $ TE.encodeUtf8 components
   in TE.decodeUtf8 $ B16.encode hashBytes

cachedRender :: CacheConfig -> Text -> (Text -> IO Text) -> IO Text
cachedRender cfg content render = do
  let key = buildCacheKey cfg content
      dir = cacheDir cfg
      path = dir </> T.unpack (T.take 32 key) <.> "html"

  createDirectoryIfMissing True dir

  exists <- doesFileExist path
  if exists
    then TIO.readFile path `catch` handleReadError path render content
    else writeAtomically dir path render content

handleReadError :: FilePath -> (Text -> IO Text) -> Text -> SomeException -> IO Text
handleReadError path render content _ = do
  writeAtomically (takeDirectory path) path render content
  where
    takeDirectory = reverse . dropWhile (/= '/') . reverse

writeAtomically :: FilePath -> FilePath -> (Text -> IO Text) -> Text -> IO Text
writeAtomically dir finalPath render content = do
  result <- render content
  (tmpPath, tmpHandle) <- openTempFile dir "cache.tmp"
  TIO.hPutStr tmpHandle result
  hClose tmpHandle
  renameFile tmpPath finalPath `catch` ignoreRenameError
  pure result
  where
    ignoreRenameError :: SomeException -> IO ()
    ignoreRenameError _ = pure ()
