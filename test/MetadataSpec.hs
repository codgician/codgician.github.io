{-# LANGUAGE OverloadedStrings #-}

module MetadataSpec (spec) where

import Content.Metadata
import Content.Types (RenderFeatures (..))
import Data.Aeson (Value (..))
import Data.Aeson.Key (fromString)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Bifunctor (first)
import Hakyll (Metadata)
import Test.Hspec

spec :: Spec
spec = describe "Content.Metadata" $ do
  describe "metadataBool" $ do
    it "accepts true case-insensitively" $ do
      metadataBool "math" (metadata [("math", String "true")]) `shouldBe` True
      metadataBool "math" (metadata [("math", String "TRUE")]) `shouldBe` True

    it "defaults missing values to false" $
      metadataBool "math" (metadata []) `shouldBe` False

  describe "featuresFromMetadata" $
    it "reads render feature flags" $
      featuresFromMetadata (metadata [("math", String "true"), ("mermaid", String "true"), ("toc", String "false")])
        `shouldBe` RenderFeatures True True False

  describe "templateFromMetadata" $
    it "defaults to page" $
      templateFromMetadata (metadata []) `shouldBe` "page"

  describe "slideLevelFromMetadata" $
    it "defaults to 2" $
      slideLevelFromMetadata (metadata []) `shouldBe` 2

metadata :: [(String, Value)] -> Metadata
metadata = KeyMap.fromList . map (first fromString)
