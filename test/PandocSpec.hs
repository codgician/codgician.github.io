{-# LANGUAGE OverloadedStrings #-}

module PandocSpec (spec) where

import Compiler.Pandoc (transformWithRenderers)
import Compiler.RenderContext
import Content.Types (RenderFeatures (..))
import Control.Exception (bracket)
import Control.Monad (void)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec
import Text.Pandoc.Definition

spec :: Spec
spec = describe "Compiler.Pandoc" $ do
  it "uses injected math renderer when math is enabled" $ do
    Pandoc _ blocks <- transformWithRenderers mathContext $ Pandoc nullMeta [Para [Math InlineMath "x"]]
    blocks `shouldBe` [Para [RawInline (Format "html") "<math>x</math>"]]

  it "leaves math untouched when math is disabled" $ do
    Pandoc _ blocks <- transformWithRenderers disabledContext $ Pandoc nullMeta [Para [Math InlineMath "x"]]
    blocks `shouldBe` [Para [Math InlineMath "x"]]

  it "uses injected Mermaid and TikZ renderers for matching code blocks" $ do
    Pandoc _ blocks <-
      transformWithRenderers diagramContext $
        Pandoc
          nullMeta
          [ CodeBlock ("", ["mermaid"], []) "graph TD; A-->B",
            CodeBlock ("", ["tikz"], []) "\\draw (0,0) -- (1,1);"
          ]
    blocks
      `shouldBe` [ RawBlock (Format "html") "<div class=\"mermaid\"><svg>graph TD; A-->B</svg></div>",
                   RawBlock (Format "html") "<div class=\"tikz\"><svg>\\draw (0,0) -- (1,1);</svg></div>"
                 ]

  describe "defaultPandocRenderContext" $ do
    it "succeeds with all features disabled without requiring any env vars" $
      withTempEnvs allRendererEnvVars Nothing $
        void $
          defaultPandocRenderContext (RenderFeatures False False False False)

    it "succeeds with math-only features when only KATEX_VERSION is set" $
      withTempEnvs nonKatexEnvVars Nothing $
        withTempEnv "KATEX_VERSION" (Just "0.16.0") $
          void $
            defaultPandocRenderContext (RenderFeatures True False False False)

mathContext :: PandocRenderContext
mathContext =
  PandocRenderContext
    { pandocRenderFeatures = RenderFeatures True False False False,
      pandocRenderers =
        PandocRenderers
          { pandocMathHtml = \_ content -> pure $ "<math>" <> content <> "</math>",
            pandocMermaidHtml = \content -> pure $ "<svg>" <> content <> "</svg>",
            pandocTikZHtml = \content -> pure $ "<div class=\"tikz\"><svg>" <> content <> "</svg></div>"
          }
    }

disabledContext :: PandocRenderContext
disabledContext = mathContext {pandocRenderFeatures = RenderFeatures False False False False}

diagramContext :: PandocRenderContext
diagramContext = mathContext {pandocRenderFeatures = RenderFeatures False True True False}

allRendererEnvVars :: [String]
allRendererEnvVars = ["KATEX_VERSION", "MERMAID_VERSION", "TIKZ_VERSION", "PUPPETEER_CONFIG", "MERMAID_CONFIG"]

nonKatexEnvVars :: [String]
nonKatexEnvVars = ["MERMAID_VERSION", "TIKZ_VERSION", "PUPPETEER_CONFIG", "MERMAID_CONFIG"]

-- | Temporarily set or unset a single env var, restoring the original value after the action.
withTempEnv :: String -> Maybe String -> IO a -> IO a
withTempEnv name newVal action =
  bracket
    (lookupEnv name <* applyEnv name newVal)
    (applyEnv name)
    (const action)
  where
    applyEnv k Nothing = unsetEnv k
    applyEnv k (Just v) = setEnv k v

-- | Temporarily unset (or set) a list of env vars, restoring originals after the action.
withTempEnvs :: [String] -> Maybe String -> IO a -> IO a
withTempEnvs [] _ action = action
withTempEnvs (k : ks) val action = withTempEnv k val (withTempEnvs ks val action)
