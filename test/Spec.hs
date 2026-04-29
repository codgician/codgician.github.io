module Main (main) where

import qualified ArtifactCacheSpec
import qualified ConfigSpec
import qualified HakyllRecoverySpec
import qualified KaTeXSpec
import qualified MermaidSpec
import qualified MetadataSpec
import qualified PaginateSpec
import qualified PandocSpec
import qualified RenderEnvironmentSpec
import qualified RoutesSpec
import Test.Hspec
import qualified TikZSpec
import qualified VendorIconsSpec

main :: IO ()
main = hspec $ do
  ArtifactCacheSpec.spec
  ConfigSpec.spec
  HakyllRecoverySpec.spec
  KaTeXSpec.spec
  MermaidSpec.spec
  MetadataSpec.spec
  PaginateSpec.spec
  PandocSpec.spec
  RenderEnvironmentSpec.spec
  RoutesSpec.spec
  TikZSpec.spec
  VendorIconsSpec.spec
