{-# LANGUAGE OverloadedStrings #-}

module Compiler.Mermaid
  ( renderMermaidDual,
    cachedMermaidDual,
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

-- | Get Mermaid version from environment (required for cache key)
getMermaidVersion :: IO Text
getMermaidVersion = do
  ver <- lookupEnv "MERMAID_VERSION"
  case ver of
    Just v -> pure $ T.pack v
    Nothing -> error "MERMAID_VERSION environment variable is not set"

-- | Get puppeteer config path from environment
getPuppeteerConfig :: IO (Maybe String)
getPuppeteerConfig = lookupEnv "PUPPETEER_CONFIG"

-- | Render Mermaid diagram to SVG with specified theme
renderMermaidTheme :: Text -> Text -> IO Text
renderMermaidTheme theme content = withSystemTempDirectory "mermaid" $ \tmpDir -> do
  let inputFile = tmpDir </> "input.mmd"
      outputFile = tmpDir </> "output.svg"
  TIO.writeFile inputFile content
  puppeteerConfig <- getPuppeteerConfig
  let baseArgs =
        [ "-i",
          inputFile,
          "-o",
          outputFile,
          "-t",
          T.unpack theme,
          "-b",
          "transparent"
        ]
      args = case puppeteerConfig of
        Just cfg -> "-p" : cfg : baseArgs
        Nothing -> baseArgs
  callProcess "mmdc" args
  TIO.readFile outputFile

-- | Render both light and dark theme SVGs, wrapped for CSS toggling
-- Each SVG gets a unique ID prefix to avoid style conflicts
renderMermaidDual :: Text -> IO Text
renderMermaidDual content = do
  lightSvg <- renderMermaidTheme "default" content
  darkSvg <- renderMermaidTheme "dark" content
  let lightSvg' = T.replace "my-svg" "mermaid-svg-light" lightSvg
      darkSvg' = T.replace "my-svg" "mermaid-svg-dark" darkSvg
  pure $ wrapDualSvg lightSvg' darkSvg'

-- | Wrap two SVGs with CSS classes for theme toggling
wrapDualSvg :: Text -> Text -> Text
wrapDualSvg lightSvg darkSvg =
  T.concat
    [ "<div class=\"mermaid-light\">",
      lightSvg,
      "</div>",
      "<div class=\"mermaid-dark\">",
      darkSvg,
      "</div>"
    ]

-- | Cached dual-theme Mermaid rendering
cachedMermaidDual :: Text -> Text -> IO Text
cachedMermaidDual filterVer content = do
  mermaidVer <- getMermaidVersion
  let cfg =
        CacheConfig
          { cacheDir = "_artifacts/mermaid",
            toolName = "mermaid",
            toolVersion = mermaidVer,
            toolOptions = "dual-theme",
            filterVersion = filterVer
          }
  cachedRender cfg content renderMermaidDual
