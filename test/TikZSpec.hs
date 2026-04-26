{-# LANGUAGE OverloadedStrings #-}

module TikZSpec (spec) where

import Compiler.TikZ (tikzCacheInput, tikzDocument, wrapTikzSvg)
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec = describe "Compiler.TikZ" $ do
  describe "tikzDocument" $
    it "wraps a TikZ picture in a standalone LaTeX document" $
      T.unpack (tikzDocument "\\draw (0,0) -- (1,1);")
        `shouldContain` "\\begin{tikzpicture}"

  describe "tikzCacheInput" $
    it "includes the LaTeX wrapper so wrapper changes invalidate cached SVGs" $
      T.unpack (tikzCacheInput "\\draw (0,0) -- (1,1);")
        `shouldContain` "\\documentclass[tikz,border=2pt]{standalone}"

  describe "wrapTikzSvg" $
    it "embeds rendered SVG in a TikZ container" $
      wrapTikzSvg "<svg><path /></svg>"
        `shouldBe` "<div class=\"tikz\"><svg><path /></svg></div>"
