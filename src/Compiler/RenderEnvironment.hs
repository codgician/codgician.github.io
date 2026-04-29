{-# LANGUAGE OverloadedStrings #-}

module Compiler.RenderEnvironment
  ( RenderConfigFile (..),
    loadRequiredToolVersion,
    loadOptionalRenderConfig,
  )
where

import Compiler.ArtifactCache (ToolVersion (..))
import Crypto.Hash.SHA256 (hash)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Environment (lookupEnv)

data RenderConfigFile = RenderConfigFile
  { configPath :: FilePath,
    configDigest :: T.Text
  }
  deriving (Eq, Show)

loadRequiredToolVersion :: String -> IO ToolVersion
loadRequiredToolVersion name = do
  value <- lookupEnv name
  case value of
    Just version -> pure $ ToolVersion $ T.pack version
    Nothing -> error $ name <> " environment variable is not set"

loadOptionalRenderConfig :: String -> IO (Maybe RenderConfigFile)
loadOptionalRenderConfig name = do
  value <- lookupEnv name
  traverse loadConfigFile value

loadConfigFile :: FilePath -> IO RenderConfigFile
loadConfigFile path = do
  bytes <- BS.readFile path
  pure RenderConfigFile {configPath = path, configDigest = digestText bytes}

digestText :: BS.ByteString -> T.Text
digestText = TE.decodeUtf8 . B16.encode . hash
