{-# LANGUAGE OverloadedStrings #-}

module TikZSpec (spec) where

import Compiler.TikZ (tikzCacheInput, tikzDocument, wrapTikzSvg)
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec = describe "Compiler.TikZ" $ do
  describe "tikzDocument" $ do
    it "wraps a TikZ picture in a standalone LaTeX document" $
      T.unpack (tikzDocument "\\draw (0,0) -- (1,1);")
        `shouldContain` "\\begin{tikzpicture}"

    it "pins sf-family to a single design size to keep glyphs consistent" $ do
      let doc = T.unpack (tikzDocument "\\draw (0,0) -- (1,1);")
      doc `shouldContain` "\\usepackage{lmodern}"
      doc `shouldContain` "\\DeclareFontShape{T1}{lmssone}{m}{n}{<-> ec-lmss10}{}"
      doc `shouldContain` "\\renewcommand{\\sfdefault}{lmssone}"

    it "keeps math in the default math font while text labels remain sans-serif" $ do
      let doc = T.unpack (tikzDocument "\\node {$x$};")
      doc `shouldContain` "\\sffamily"
      doc `shouldNotContain` "\\usepackage{sansmath}"
      doc `shouldNotContain` "\\sansmath"

  describe "tikzCacheInput" $
    it "includes the LaTeX wrapper so wrapper changes invalidate cached SVGs" $
      T.unpack (tikzCacheInput "\\draw (0,0) -- (1,1);")
        `shouldContain` "\\documentclass[tikz,border=2pt]{standalone}"

  describe "wrapTikzSvg" $
    it "embeds rendered SVG in a TikZ container" $
      wrapTikzSvg "<svg><path /></svg>"
        `shouldBe` "<div class=\"tikz\"><svg><path /></svg></div>"
