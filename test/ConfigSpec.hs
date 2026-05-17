{-# LANGUAGE OverloadedStrings #-}

module ConfigSpec (spec) where

import Config
import Context (activeNavUrl, yearStartFlags)
import Content.Types (LangCode (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec = describe "Config" $ do
  describe "getTrans" $ do
    let langs = [Language (LangCode "en") "English", Language (LangCode "zh") "中文"]
    let translated = Translated $ Map.fromList [("en", "Hello"), ("zh", "你好")]

    it "returns translation for requested language" $ do
      getTrans langs (LangCode "en") translated `shouldBe` "Hello"
      getTrans langs (LangCode "zh") translated `shouldBe` "你好"

    it "falls back to default language when translation missing" $ do
      let partial = Translated $ Map.fromList [("en", "Hello")]
      getTrans langs (LangCode "zh") partial `shouldBe` "Hello"

    it "returns empty string when no translations available" $ do
      let empty = Translated Map.empty
      getTrans langs (LangCode "en") empty `shouldBe` ""

  describe "getTransList" $ do
    let langs = [Language (LangCode "en") "English", Language (LangCode "zh") "中文"]
    let phrases = TranslatedList $ Map.fromList [("en", ["Hi", "Hello"]), ("zh", ["嗨", "你好"])]

    it "returns list for requested language" $ do
      getTransList langs (LangCode "en") phrases `shouldBe` ["Hi", "Hello"]
      getTransList langs (LangCode "zh") phrases `shouldBe` ["嗨", "你好"]

    it "falls back to default language when missing" $ do
      let partial = TranslatedList $ Map.fromList [("en", ["Hi"])]
      getTransList langs (LangCode "zh") partial `shouldBe` ["Hi"]

  describe "defaultLang" $ do
    it "returns first language code" $ do
      let langs = [Language (LangCode "zh") "中文", Language (LangCode "en") "English"]
      defaultLang langs `shouldBe` LangCode "zh"

    it "returns 'en' for empty list" $
      defaultLang [] `shouldBe` LangCode "en"

  describe "postsPerPage" $ do
    it "returns configured value" $ do
      let cfg = minimalConfig {pagination = Just (PaginationConfig (PaginationItemConfig 5) (PaginationItemConfig 6))}
      postsPerPage cfg `shouldBe` 5

    it "returns default 10 when not configured" $
      postsPerPage minimalConfig `shouldBe` 10

  describe "slidesPerPage" $ do
    it "returns configured value" $ do
      let cfg = minimalConfig {pagination = Just (PaginationConfig (PaginationItemConfig 5) (PaginationItemConfig 8))}
      slidesPerPage cfg `shouldBe` 8

    it "returns default 12 when not configured" $
      slidesPerPage minimalConfig `shouldBe` 12

  describe "activeNavUrl" $ do
    it "marks matching section navigation as active" $ do
      activeNavUrl (Just "posts/hello-world/") "posts/" `shouldBe` True
      activeNavUrl (Just "slides/") "slides/" `shouldBe` True

    it "does not mark unrelated navigation as active" $ do
      activeNavUrl (Just "posts/hello-world/") "slides/" `shouldBe` False
      activeNavUrl (Just "about/") "posts/" `shouldBe` False

    it "does not mark navigation active without a current path" $
      activeNavUrl Nothing "posts/" `shouldBe` False

  describe "yearStartFlags" $ do
    it "marks the first post of every non-initial year group" $
      yearStartFlags [["2026-a", "2026-b"], ["2025-a"], ["2024-a", "2024-b"] :: [T.Text]]
        `shouldBe` [[False, False], [True], [True, False]]

    it "handles empty groups without adding flags" $
      yearStartFlags [[1, 2], [], [3 :: Int]] `shouldBe` [[False, False], [], [True]]

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
      languages = [Language (LangCode "en") "English"],
      author = AuthorConfig "Test" Nothing,
      navigation = [],
      social = [],
      friends = [],
      feed = FeedConfig (Translated Map.empty) (Translated Map.empty) 10,
      pagination = Nothing
    }
