module Main (main) where

import qualified Site.HtmlSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  Site.HtmlSpec.spec
