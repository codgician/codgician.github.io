{-# LANGUAGE OverloadedStrings #-}

module RenderEnvironmentSpec (spec) where

import Compiler.ArtifactCache (ToolVersion (..))
import Compiler.RenderEnvironment
import Control.Exception (finally)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = describe "Compiler.RenderEnvironment" $ do
  it "loads a required tool version" $ do
    setEnv "KATEX_VERSION" "0.16.22"
    loadRequiredToolVersion "KATEX_VERSION" `shouldReturn` ToolVersion "0.16.22"

  it "fails clearly when a required version is missing" $ do
    unsetEnv "KATEX_VERSION"
    loadRequiredToolVersion "KATEX_VERSION" `shouldThrow` errorCall "KATEX_VERSION environment variable is not set"

  it "includes config file digests for optional renderer configs" $
    withSystemTempDirectory "render-env" $ \dir -> do
      let mermaidConfig = dir </> "mermaid.json"
      TIO.writeFile mermaidConfig "{\"theme\":\"default\"}"
      setEnv "MERMAID_CONFIG" mermaidConfig
      let cleanup = unsetEnv "MERMAID_CONFIG"
      flip finally cleanup $ do
        config <- loadOptionalRenderConfig "MERMAID_CONFIG"
        fmap configPath config `shouldBe` Just mermaidConfig
        fmap configDigest config `shouldSatisfy` maybe False (not . T.null)
