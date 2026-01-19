{-# LANGUAGE OverloadedStrings #-}

module Compiler.Cache
  ( cachedRender,
    CacheConfig (..),
  )
where

import Crypto.Hash.SHA256 (hash)
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), (<.>))

-- | Cache configuration
data CacheConfig = CacheConfig
  { cacheDir :: FilePath,
    toolName :: Text,
    toolVersion :: Text,
    toolOptions :: Text,
    filterVersion :: Text
  }

-- | Build a deterministic cache key from all inputs
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

-- | Cached rendering with content-addressed storage
cachedRender :: CacheConfig -> Text -> (Text -> IO Text) -> IO Text
cachedRender cfg content render = do
  let key = buildCacheKey cfg content
      path = cacheDir cfg </> T.unpack (T.take 32 key) <.> "html"

  exists <- doesFileExist path
  if exists
    then TIO.readFile path
    else do
      createDirectoryIfMissing True (cacheDir cfg)
      result <- render content
      TIO.writeFile path result
      pure result
