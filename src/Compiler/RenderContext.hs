{-# LANGUAGE OverloadedStrings #-}

module Compiler.RenderContext
  ( PandocRenderContext (..),
    PandocRenderers (..),
    defaultPandocRenderContext,
  )
where

import Compiler.ArtifactCache (ArtifactCache (..), ArtifactRoot (..), CacheSchema (..), renderCached)
import Compiler.KaTeX (KaTeXInput (..), katexArtifactRendererFor)
import Compiler.Mermaid (MermaidSource (..), mermaidArtifactRendererFor)
import Compiler.RenderEnvironment (loadOptionalRenderConfig, loadRequiredToolVersion)
import Compiler.TikZ (TikZSource (..), tikzArtifactRendererFor)
import Content.Types (RenderFeatures (..))
import Data.Text (Text)
import System.IO (hPutStrLn, stderr)
import Text.Pandoc.Definition (MathType)

data PandocRenderers = PandocRenderers
  { pandocMathHtml :: MathType -> Text -> IO Text,
    pandocMermaidHtml :: Text -> IO Text,
    pandocTikZHtml :: Text -> IO Text
  }

data PandocRenderContext = PandocRenderContext
  { pandocRenderFeatures :: RenderFeatures,
    pandocRenderers :: PandocRenderers
  }

-- | Build a PandocRenderContext loading only the env vars required by enabled features.
defaultPandocRenderContext :: RenderFeatures -> IO PandocRenderContext
defaultPandocRenderContext features = do
  mathRenderer <-
    if renderMath features
      then do
        katexVer <- loadRequiredToolVersion "KATEX_VERSION"
        pure $ \mathType content ->
          renderCached defaultArtifactCache (katexArtifactRendererFor katexVer) (KaTeXInput mathType content)
      else pure $ \_ _ -> fail "Math renderer invoked but renderMath is disabled"
  mermaidRenderer <-
    if renderMermaid features
      then do
        mermaidVer <- loadRequiredToolVersion "MERMAID_VERSION"
        puppeteerCfg <- loadOptionalRenderConfig "PUPPETEER_CONFIG"
        mermaidCfg <- loadOptionalRenderConfig "MERMAID_CONFIG"
        pure $ \content ->
          renderCached defaultArtifactCache (mermaidArtifactRendererFor mermaidVer puppeteerCfg mermaidCfg) (MermaidSource content)
      else pure $ \_ -> fail "Mermaid renderer invoked but renderMermaid is disabled"
  tikzRenderer <-
    if renderTikZ features
      then do
        tikzVer <- loadRequiredToolVersion "TIKZ_VERSION"
        pure $ \content ->
          renderCached defaultArtifactCache (tikzArtifactRendererFor tikzVer) (TikZSource content)
      else pure $ \_ -> fail "TikZ renderer invoked but renderTikZ is disabled"
  pure
    PandocRenderContext
      { pandocRenderFeatures = features,
        pandocRenderers =
          PandocRenderers
            { pandocMathHtml = mathRenderer,
              pandocMermaidHtml = mermaidRenderer,
              pandocTikZHtml = tikzRenderer
            }
      }

defaultArtifactCache :: ArtifactCache
defaultArtifactCache =
  ArtifactCache
    { artifactRoot = ArtifactRoot "_artifacts",
      artifactSchema = CacheSchema "v2",
      artifactReporter = \ev -> hPutStrLn stderr $ "ArtifactCache: " <> show ev
    }
