{-# LANGUAGE OverloadedStrings #-}

module Compiler.Mermaid
  ( renderMermaid,
    cachedMermaid,
  )
where

import Compiler.Cache (CacheConfig (..), cachedRender)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)

-- | Get Mermaid version from environment
getMermaidVersion :: IO Text
getMermaidVersion = do
  ver <- lookupEnv "MERMAID_VERSION"
  pure $ maybe "unknown" T.pack ver

-- | Render Mermaid diagram to SVG
renderMermaid :: Text -> IO Text
renderMermaid content = withSystemTempDirectory "mermaid" $ \tmpDir -> do
  let inputFile = tmpDir </> "input.mmd"
      outputFile = tmpDir </> "output.svg"
  TIO.writeFile inputFile content
  callProcess
    "mmdc"
    [ "-i",
      inputFile,
      "-o",
      outputFile,
      "-t",
      "neutral",
      "-b",
      "transparent"
    ]
  TIO.readFile outputFile

-- | Cached Mermaid rendering
cachedMermaid :: Text -> Text -> IO Text
cachedMermaid filterVer content = do
  mermaidVer <- getMermaidVersion
  let cfg =
        CacheConfig
          { cacheDir = "_artifacts/mermaid",
            toolName = "mermaid",
            toolVersion = mermaidVer,
            toolOptions = "-t neutral -b transparent",
            filterVersion = filterVer
          }
  cachedRender cfg content renderMermaid
