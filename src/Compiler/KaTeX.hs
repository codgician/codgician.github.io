{-# LANGUAGE OverloadedStrings #-}

module Compiler.KaTeX
  ( renderKaTeX,
    cachedKaTeX,
  )
where

import Compiler.Cache (CacheConfig (..), cachedRender)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (lookupEnv)
import System.IO (hClose, hFlush)
import System.Process

-- | Get KaTeX version from environment (required for cache key)
getKaTeXVersion :: IO Text
getKaTeXVersion = do
  ver <- lookupEnv "KATEX_VERSION"
  case ver of
    Just v -> pure $ T.pack v
    Nothing -> error "KATEX_VERSION environment variable is not set"

-- | Render LaTeX to HTML using KaTeX CLI
renderKaTeX :: Bool -> Text -> IO Text
renderKaTeX displayMode content = do
  let args =
        ["--strict", "--trust"]
          ++ ["--display-mode" | displayMode]
  (Just stdin', Just stdout', _, ph) <-
    createProcess
      (proc "katex" args)
        { std_in = CreatePipe,
          std_out = CreatePipe
        }
  TIO.hPutStr stdin' content
  hFlush stdin'
  hClose stdin'
  result <- TIO.hGetContents stdout'
  _ <- waitForProcess ph
  pure result

-- | Cached KaTeX rendering
cachedKaTeX :: Text -> Bool -> Text -> IO Text
cachedKaTeX filterVer displayMode content = do
  katexVer <- getKaTeXVersion
  let cfg =
        CacheConfig
          { cacheDir = "_artifacts/katex",
            toolName = "katex",
            toolVersion = katexVer,
            toolOptions = if displayMode then "--display-mode" else "",
            filterVersion = filterVer
          }
  cachedRender cfg content (renderKaTeX displayMode)
