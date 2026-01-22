module Main (main) where

import qualified ConfigSpec
import qualified PaginateSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  ConfigSpec.spec
  PaginateSpec.spec
