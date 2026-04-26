module Main (main) where

import qualified ConfigSpec
import qualified MetadataSpec
import qualified PaginateSpec
import qualified RoutesSpec
import Test.Hspec
import qualified VendorIconsSpec

main :: IO ()
main = hspec $ do
  ConfigSpec.spec
  MetadataSpec.spec
  PaginateSpec.spec
  RoutesSpec.spec
  VendorIconsSpec.spec
