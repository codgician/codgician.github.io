{-# LANGUAGE OverloadedStrings #-}

module MermaidSpec (spec) where

import Compiler.ArtifactCache
import Compiler.Mermaid
import Compiler.RenderEnvironment (RenderConfigFile (..))
import Test.Hspec

spec :: Spec
spec = describe "Compiler.Mermaid" $
  it "includes config digests in recipe options" $ do
    let cache = ArtifactCache (ArtifactRoot "_artifacts") (CacheSchema "v1") (const $ pure ())
        rendererA = mermaidArtifactRendererFor mermaidVersion puppeteerConfigA mermaidConfigA
        rendererB = mermaidArtifactRendererFor mermaidVersion puppeteerConfigB mermaidConfigB
    cacheKey cache rendererA (MermaidSource "graph TD; A-->B")
      `shouldNotBe` cacheKey cache rendererB (MermaidSource "graph TD; A-->B")

mermaidVersion :: ToolVersion
mermaidVersion = ToolVersion "11.4.1"

puppeteerConfigA :: Maybe RenderConfigFile
puppeteerConfigA = Just $ RenderConfigFile "puppeteer.json" "digest-a"

mermaidConfigA :: Maybe RenderConfigFile
mermaidConfigA = Just $ RenderConfigFile "mermaid.json" "digest-a"

puppeteerConfigB :: Maybe RenderConfigFile
puppeteerConfigB = Just $ RenderConfigFile "puppeteer.json" "digest-b"

mermaidConfigB :: Maybe RenderConfigFile
mermaidConfigB = Just $ RenderConfigFile "mermaid.json" "digest-b"
