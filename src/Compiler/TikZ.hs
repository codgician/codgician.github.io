{-# LANGUAGE OverloadedStrings #-}

module Compiler.TikZ
  ( TikZDocument (..),
    TikZSource (..),
    renderTikZ,
    tikzArtifactRendererFor,
    tikzDocument,
    wrapTikzSvg,
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
import Control.Exception (IOException, catch, throwIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (cwd), proc, readCreateProcessWithExitCode)

newtype TikZSource = TikZSource Text deriving (Eq, Show)

newtype TikZDocument = TikZDocument Text deriving (Eq, Show)

tikzRecipeVersion :: RecipeVersion
tikzRecipeVersion = RecipeVersion "tikz-recipe-v1"

tikzRendererId :: RendererId
tikzRendererId =
  case mkRendererId "tikz" of
    Right rendererId -> rendererId
    Left err -> error $ "Invalid built-in TikZ renderer ID: " <> show err

tikzDocument :: TikZSource -> TikZDocument
tikzDocument (TikZSource content) =
  TikZDocument $
    T.unlines
      [ "\\documentclass[tikz,border=2pt]{standalone}",
        "\\usepackage[T1]{fontenc}",
        "\\usepackage{lmodern}",
        "\\usepackage{tikz}",
        "\\DeclareFontFamily{T1}{lmssone}{}",
        "\\DeclareFontShape{T1}{lmssone}{m}{n}{<-> ec-lmss10}{}",
        "\\renewcommand{\\sfdefault}{lmssone}",
        "\\begin{document}",
        "\\sffamily",
        "\\begin{tikzpicture}",
        content,
        "\\end{tikzpicture}",
        "\\end{document}"
      ]

tikzArtifactRendererFor :: ToolVersion -> ArtifactRenderer TikZSource
tikzArtifactRendererFor toolVersion =
  ArtifactRenderer
    { artifactRecipe =
        RendererRecipe
          { recipeId = tikzRendererId,
            recipeToolVersion = toolVersion,
            recipeVersion = tikzRecipeVersion,
            recipeOptions = []
          },
      artifactCacheInput = \src ->
        let TikZDocument doc = tikzDocument src
         in CacheInput doc,
      artifactRender = renderTikZ
    }

renderTikZ :: TikZSource -> IO Text
renderTikZ = renderTikZDocument . tikzDocument

wrapTikzSvg :: Text -> Text
wrapTikzSvg svg = "<div class=\"tikz\">" <> T.strip (stripXmlPreamble svg) <> "</div>"

renderTikZDocument :: TikZDocument -> IO Text
renderTikZDocument (TikZDocument document) =
  withSystemTempDirectory "tikz" $ \tmpDir -> do
    let texPath = tmpDir </> "diagram.tex"
        dviPath = tmpDir </> "diagram.dvi"
        svgPath = tmpDir </> "diagram.svg"
    TIO.writeFile texPath document
    runTool tmpDir "latex" ["-interaction=nonstopmode", "-halt-on-error", "diagram.tex"]
    dviExists <- doesFileExist dviPath
    if dviExists
      then runTool tmpDir "dvisvgm" ["--no-fonts=1", "--exact", "--output=diagram.svg", "diagram.dvi"]
      else failWithContext "latex did not produce diagram.dvi" ""
    svg <- TIO.readFile svgPath
    pure $ wrapTikzSvg svg

runTool :: FilePath -> String -> [String] -> IO ()
runTool workingDir command args = do
  let process = (proc command args) {cwd = Just workingDir}
  result <- readCreateProcessWithExitCode process "" `catch` annotateSpawnFailure command args
  case result of
    (ExitSuccess, _, _) -> pure ()
    (ExitFailure code, stdoutText, stderrText) ->
      failWithContext
        (command <> " failed with exit code " <> show code)
        (unlines ["cwd: " <> workingDir, "command: " <> unwords (command : args), "stdout:", stdoutText, "stderr:", stderrText])

annotateSpawnFailure :: String -> [String] -> IOException -> IO (ExitCode, String, String)
annotateSpawnFailure command args ex =
  throwIO $ userError $ "Failed to start " <> unwords (command : args) <> ": " <> show ex

failWithContext :: String -> String -> IO a
failWithContext message details = fail $ message <> "\n" <> details

stripXmlPreamble :: Text -> Text
stripXmlPreamble svg =
  T.unlines $ filter (not . isXmlHeader) $ T.lines svg
  where
    isXmlHeader line =
      "<?xml" `T.isPrefixOf` T.strip line
        || "<!DOCTYPE" `T.isPrefixOf` T.strip line
