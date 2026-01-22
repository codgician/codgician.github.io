{-# LANGUAGE OverloadedStrings #-}

module ConfigSpec (spec) where

import Config
import qualified Data.Map.Strict as Map
import Test.Hspec

spec :: Spec
spec = describe "Config" $ do
  describe "getTrans" $ do
    let langs = [Language "en" "English", Language "zh" "中文"]
    let translated = Translated $ Map.fromList [("en", "Hello"), ("zh", "你好")]

    it "returns translation for requested language" $ do
      getTrans langs "en" translated `shouldBe` "Hello"
      getTrans langs "zh" translated `shouldBe` "你好"

    it "falls back to default language when translation missing" $ do
      let partial = Translated $ Map.fromList [("en", "Hello")]
      getTrans langs "zh" partial `shouldBe` "Hello"

    it "returns empty string when no translations available" $ do
      let empty = Translated Map.empty
      getTrans langs "en" empty `shouldBe` ""

  describe "getTransList" $ do
    let langs = [Language "en" "English", Language "zh" "中文"]
    let phrases = TranslatedList $ Map.fromList [("en", ["Hi", "Hello"]), ("zh", ["嗨", "你好"])]

    it "returns list for requested language" $ do
      getTransList langs "en" phrases `shouldBe` ["Hi", "Hello"]
      getTransList langs "zh" phrases `shouldBe` ["嗨", "你好"]

    it "falls back to default language when missing" $ do
      let partial = TranslatedList $ Map.fromList [("en", ["Hi"])]
      getTransList langs "zh" partial `shouldBe` ["Hi"]

  describe "defaultLang" $ do
    it "returns first language code" $ do
      let langs = [Language "zh" "中文", Language "en" "English"]
      defaultLang langs `shouldBe` "zh"

    it "returns 'en' for empty list" $ do
      defaultLang [] `shouldBe` "en"

  describe "postsPerPage" $ do
    it "returns configured value" $ do
      let cfg = minimalConfig {pagination = Just (PaginationConfig (PaginationItemConfig 5) (PaginationItemConfig 6))}
      postsPerPage cfg `shouldBe` 5

    it "returns default 10 when not configured" $ do
      postsPerPage minimalConfig `shouldBe` 10

  describe "slidesPerPage" $ do
    it "returns configured value" $ do
      let cfg = minimalConfig {pagination = Just (PaginationConfig (PaginationItemConfig 5) (PaginationItemConfig 8))}
      slidesPerPage cfg `shouldBe` 8

    it "returns default 12 when not configured" $ do
      slidesPerPage minimalConfig `shouldBe` 12

-- | Minimal config for testing
minimalConfig :: SiteConfig
minimalConfig =
  SiteConfig
    { site =
        SiteInfo
          { title = Translated Map.empty,
            subtitle = Translated Map.empty,
            typewriterPhrases = TranslatedList Map.empty,
            baseUrl = "https://example.com",
            copyright = Translated Map.empty,
            license = Nothing
          },
      languages = [Language "en" "English"],
      author = AuthorConfig "Test" Nothing,
      navigation = [],
      social = [],
      friends = [],
      feed = FeedConfig (Translated Map.empty) (Translated Map.empty) 10,
      pagination = Nothing
    }
