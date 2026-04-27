{-# LANGUAGE OverloadedStrings #-}

module Compiler.TikZ
  ( cachedTikZ,
    renderTikZ,
    tikzCacheInput,
    tikzDocument,
    wrapTikzSvg,
  )
where

import Compiler.Cache (CacheConfig (..), cachedRender)
import Control.Exception (SomeException, catch, throwIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (cwd), proc, readCreateProcessWithExitCode)

tikzWrapperVersion :: Text
tikzWrapperVersion = "standalone-v3"

getTikZVersion :: IO Text
getTikZVersion = do
  ver <- lookupEnv "TIKZ_VERSION"
  case ver of
    Just v -> pure $ T.pack v
    Nothing -> error "TIKZ_VERSION environment variable is not set"

tikzDocument :: Text -> Text
tikzDocument content =
  T.unlines
    [ "\\documentclass[tikz,border=2pt]{standalone}",
      "\\usepackage[T1]{fontenc}",
      "\\usepackage{lmodern}",
      "\\usepackage{sansmath}",
      "\\usepackage{tikz}",
      "\\DeclareFontFamily{T1}{lmssone}{}",
      "\\DeclareFontShape{T1}{lmssone}{m}{n}{<-> ec-lmss10}{}",
      "\\renewcommand{\\sfdefault}{lmssone}",
      "\\sansmath",
      "\\begin{document}",
      "\\sffamily",
      "\\begin{tikzpicture}",
      content,
      "\\end{tikzpicture}",
      "\\end{document}"
    ]

tikzCacheInput :: Text -> Text
tikzCacheInput content =
  T.intercalate
    "\n"
    [ "% wrapper=" <> tikzWrapperVersion,
      tikzDocument content
    ]

renderTikZ :: Text -> IO Text
renderTikZ = renderTikZDocument . tikzDocument

wrapTikzSvg :: Text -> Text
wrapTikzSvg svg = "<div class=\"tikz\">" <> T.strip (stripXmlPreamble svg) <> "</div>"

cachedTikZ :: Text -> Text -> IO Text
cachedTikZ filterVer content = do
  tikzVer <- getTikZVersion
  let cfg =
        CacheConfig
          { cacheDir = "_artifacts/tikz",
            toolName = "tikz",
            toolVersion = tikzVer,
            toolOptions = tikzWrapperVersion,
            filterVersion = filterVer
          }
  cachedRender cfg (tikzCacheInput content) renderTikZFromCacheInput

renderTikZFromCacheInput :: Text -> IO Text
renderTikZFromCacheInput = renderTikZDocument

renderTikZDocument :: Text -> IO Text
renderTikZDocument document = withSystemTempDirectory "tikz" $ \tmpDir -> do
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

annotateSpawnFailure :: String -> [String] -> SomeException -> IO (ExitCode, String, String)
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
