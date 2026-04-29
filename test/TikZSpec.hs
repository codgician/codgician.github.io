{-# LANGUAGE OverloadedStrings #-}

module TikZSpec (spec) where

import Compiler.ArtifactCache (ArtifactRenderer (..), CacheInput (..), ToolVersion (..))
import Compiler.TikZ (TikZDocument (..), TikZSource (..), tikzArtifactRendererFor, tikzDocument, wrapTikzSvg)
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec = describe "Compiler.TikZ" $ do
  describe "tikzDocument" $ do
    it "wraps a TikZ picture in a standalone LaTeX document" $
      T.unpack (documentText $ tikzDocument $ TikZSource "\\draw (0,0) -- (1,1);")
        `shouldContain` "\\begin{tikzpicture}"

    it "pins sf-family to a single design size to keep glyphs consistent" $ do
      let doc = T.unpack $ documentText $ tikzDocument $ TikZSource "\\draw (0,0) -- (1,1);"
      doc `shouldContain` "\\usepackage{lmodern}"
      doc `shouldContain` "\\DeclareFontShape{T1}{lmssone}{m}{n}{<-> ec-lmss10}{}"
      doc `shouldContain` "\\renewcommand{\\sfdefault}{lmssone}"

    it "keeps math in the default math font while text labels remain sans-serif" $ do
      let doc = T.unpack $ documentText $ tikzDocument $ TikZSource "\\node {$x$};"
      doc `shouldContain` "\\sffamily"
      doc `shouldNotContain` "\\usepackage{sansmath}"
      doc `shouldNotContain` "\\sansmath"

  describe "tikzArtifactRenderer" $
    it "uses the generated LaTeX document as cache input" $ do
      let CacheInput input = artifactCacheInput (tikzArtifactRendererFor (ToolVersion "texlive-2025")) (TikZSource "\\draw (0,0) -- (1,1);")
      T.unpack input `shouldContain` "\\documentclass[tikz,border=2pt]{standalone}"

  describe "wrapTikzSvg" $
    it "embeds rendered SVG in a TikZ container" $
      wrapTikzSvg "<svg><path /></svg>"
        `shouldBe` "<div class=\"tikz\"><svg><path /></svg></div>"

documentText :: TikZDocument -> T.Text
documentText (TikZDocument text) = text
