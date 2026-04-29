{-# LANGUAGE OverloadedStrings #-}

module KaTeXSpec (spec) where

import Compiler.ArtifactCache
import Compiler.KaTeX
import Control.Exception (SomeException, bracket, displayException)
import Data.List (isInfixOf)
import System.Directory (executable, getPermissions, setPermissions)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Text.Pandoc.Definition (MathType (..))

spec :: Spec
spec = describe "Compiler.KaTeX" $ do
  it "includes math mode in cache key material through recipe options and cache input" $ do
    let cache = ArtifactCache (ArtifactRoot "_artifacts") (CacheSchema "v1") (const $ pure ())
        renderer = katexArtifactRendererFor (ToolVersion "0.16.22")
    cacheKey cache renderer (KaTeXInput InlineMath "x")
      `shouldNotBe` cacheKey cache renderer (KaTeXInput DisplayMath "x")

  it "fails loudly when the KaTeX process exits unsuccessfully" $
    withSystemTempDirectory "fake-katex" $ \dir -> do
      let fakeKatex = dir </> "katex"
      writeFile fakeKatex "#!/bin/sh\necho partial-output\necho katex-error >&2\nexit 42\n"
      perms <- getPermissions fakeKatex
      setPermissions fakeKatex perms {executable = True}
      withTempEnv "PATH" (Just dir) $
        renderKaTeX False "x" `shouldThrow` katexFailure

katexFailure :: SomeException -> Bool
katexFailure ex =
  let msg = displayException ex
   in "katex failed with exit code 42" `isInfixOf` msg
        && "partial-output" `isInfixOf` msg
        && "katex-error" `isInfixOf` msg

withTempEnv :: String -> Maybe String -> IO a -> IO a
withTempEnv name newVal action =
  bracket
    (lookupEnv name <* applyEnv name newVal)
    (applyEnv name)
    (const action)
  where
    applyEnv k Nothing = unsetEnv k
    applyEnv k (Just v) = setEnv k v
