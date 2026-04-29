{-# LANGUAGE OverloadedStrings #-}

module Compiler.KaTeX
  ( KaTeXInput (..),
    katexArtifactRendererFor,
    renderKaTeX,
  )
where

import Compiler.ArtifactCache
  ( ArtifactRenderer (..),
    CacheInput (..),
    RecipeVersion (..),
    RendererId,
    RendererRecipe (..),
    ToolVersion,
    mkRendererId,
  )
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import System.Process (proc, readCreateProcessWithExitCode)
import Text.Pandoc.Definition (MathType (..))

data KaTeXInput = KaTeXInput MathType Text deriving (Eq, Show)

katexRecipeVersion :: RecipeVersion
katexRecipeVersion = RecipeVersion "katex-recipe-v1"

katexRendererId :: RendererId
katexRendererId =
  case mkRendererId "katex" of
    Right rendererId -> rendererId
    Left err -> error $ "Invalid built-in KaTeX renderer ID: " <> show err

katexArtifactRendererFor :: ToolVersion -> ArtifactRenderer KaTeXInput
katexArtifactRendererFor toolVersion =
  ArtifactRenderer
    { artifactRecipe =
        RendererRecipe
          { recipeId = katexRendererId,
            recipeToolVersion = toolVersion,
            recipeVersion = katexRecipeVersion,
            recipeOptions = []
          },
      artifactCacheInput = \(KaTeXInput mathType content) ->
        let mode = if mathType == DisplayMath then "display" else "inline"
         in CacheInput $ mode <> "\n" <> content,
      artifactRender = \(KaTeXInput mathType content) ->
        renderKaTeX (mathType == DisplayMath) content
    }

-- | Render LaTeX to HTML using KaTeX CLI
renderKaTeX :: Bool -> Text -> IO Text
renderKaTeX displayMode content = do
  let args =
        ["--strict", "--trust"]
          ++ ["--display-mode" | displayMode]
  (exitCode, stdoutText, stderrText) <- readCreateProcessWithExitCode (proc "katex" args) (T.unpack content)
  case exitCode of
    ExitSuccess -> pure $ T.pack stdoutText
    ExitFailure code ->
      fail $
        unlines
          [ "katex failed with exit code " <> show code,
            "command: " <> unwords ("katex" : args),
            "stdout:",
            stdoutText,
            "stderr:",
            stderrText
          ]
