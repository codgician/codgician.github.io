{-# LANGUAGE OverloadedStrings #-}

module Compiler.Mermaid
  ( MermaidSource (..),
    mermaidArtifactRendererFor,
  )
where

import Compiler.ArtifactCache
  ( ArtifactRenderer (..),
    CacheInput (..),
    RecipeOption (..),
    RecipeVersion (..),
    RendererId,
    RendererRecipe (..),
    ToolVersion,
    mkRendererId,
  )
import Compiler.RenderEnvironment (RenderConfigFile (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)

newtype MermaidSource = MermaidSource Text deriving (Eq, Show)

mermaidRecipeVersion :: RecipeVersion
mermaidRecipeVersion = RecipeVersion "mermaid-recipe-v1"

mermaidRendererId :: RendererId
mermaidRendererId =
  case mkRendererId "mermaid" of
    Right rendererId -> rendererId
    Left err -> error $ "Invalid built-in Mermaid renderer ID: " <> show err

configRecipeOptions :: Maybe RenderConfigFile -> Text -> [RecipeOption]
configRecipeOptions Nothing _ = []
configRecipeOptions (Just (RenderConfigFile path digest)) name =
  [RecipeOption $ name <> ":" <> T.pack path <> ":" <> digest]

mermaidArtifactRendererFor :: ToolVersion -> Maybe RenderConfigFile -> Maybe RenderConfigFile -> ArtifactRenderer MermaidSource
mermaidArtifactRendererFor toolVersion puppeteerCfg mermaidCfg =
  ArtifactRenderer
    { artifactRecipe =
        RendererRecipe
          { recipeId = mermaidRendererId,
            recipeToolVersion = toolVersion,
            recipeVersion = mermaidRecipeVersion,
            recipeOptions =
              [RecipeOption "dual-theme"]
                ++ configRecipeOptions puppeteerCfg "puppeteer"
                ++ configRecipeOptions mermaidCfg "mermaid"
          },
      artifactCacheInput = \(MermaidSource src) -> CacheInput src,
      artifactRender = renderMermaidDualFor puppeteerCfg mermaidCfg
    }

-- | Render Mermaid diagram to SVG with specified theme
renderMermaidTheme :: Maybe RenderConfigFile -> Maybe RenderConfigFile -> Text -> MermaidSource -> IO Text
renderMermaidTheme puppeteerCfg mermaidCfg theme (MermaidSource content) =
  withSystemTempDirectory "mermaid" $ \tmpDir -> do
    let inputFile = tmpDir </> "input.mmd"
        outputFile = tmpDir </> "output.svg"
    TIO.writeFile inputFile content
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
        withPuppeteer xs = case puppeteerCfg of
          Just cfg -> "-p" : configPath cfg : xs
          Nothing -> xs
        withMermaid xs = case mermaidCfg of
          Just cfg -> "-c" : configPath cfg : xs
          Nothing -> xs
        args = withPuppeteer $ withMermaid baseArgs
    callProcess "mmdc" args
    TIO.readFile outputFile

-- | Render both light and dark theme SVGs, wrapped for CSS toggling
renderMermaidDualFor :: Maybe RenderConfigFile -> Maybe RenderConfigFile -> MermaidSource -> IO Text
renderMermaidDualFor puppeteerCfg mermaidCfg src = do
  lightSvg <- renderMermaidTheme puppeteerCfg mermaidCfg "default" src
  darkSvg <- renderMermaidTheme puppeteerCfg mermaidCfg "dark" src
  let lightSvg' = T.replace "my-svg" "mermaid-svg-light" lightSvg
      darkSvg' = T.replace "my-svg" "mermaid-svg-dark" darkSvg
  pure $ wrapDualSvg lightSvg' darkSvg'

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
