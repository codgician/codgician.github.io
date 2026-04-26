{-# LANGUAGE OverloadedStrings #-}

module RoutesSpec (spec) where

import Content.Types
import Data.Either (isLeft)
import Hakyll (fromFilePath, toFilePath)
import Routes
import Test.Hspec

spec :: Spec
spec = describe "Routes" $ do
  describe "public URLs" $ do
    it "renders home URLs" $ do
      homeUrl (LangCode "en") `shouldBe` "/en/"
      homeUrl (LangCode "zh") `shouldBe` "/zh/"

    it "renders post URLs" $
      postUrl (LangCode "en") (Slug "hello-world") `shouldBe` "/en/posts/hello-world/"

    it "renders slide URLs" $
      slideUrl (LangCode "zh") (Slug "nix-intro") `shouldBe` "/zh/slides/nix-intro/"

    it "renders feed URLs" $
      feedUrl (LangCode "en") `shouldBe` "/en/feed.xml"

    it "renders page URLs from path segments" $
      pageUrl (LangCode "zh") ["icpc", "templates", "math"] `shouldBe` "/zh/icpc/templates/math/"

    it "renders page URL with empty segments as home URL" $
      pageUrl (LangCode "en") [] `shouldBe` "/en/"

    it "renders section index URLs" $ do
      sectionIndexUrl (LangCode "en") Posts `shouldBe` "/en/posts/"
      sectionIndexUrl (LangCode "zh") Slides `shouldBe` "/zh/slides/"

  describe "output paths" $ do
    it "renders home output paths" $ do
      homeOutputPath (LangCode "en") `shouldBe` "en/index.html"
      homeOutputPath (LangCode "zh") `shouldBe` "zh/index.html"

    it "renders post output paths" $
      postOutputPath (LangCode "en") (Slug "hello-world") `shouldBe` "en/posts/hello-world/index.html"

    it "renders slide output paths" $
      slideOutputPath (LangCode "zh") (Slug "nix-intro") `shouldBe` "zh/slides/nix-intro/index.html"

    it "renders feed output paths" $ do
      feedOutputPath (LangCode "en") `shouldBe` "en/feed.xml"
      feedOutputPath (LangCode "zh") `shouldBe` "zh/feed.xml"

    it "renders section index output paths" $ do
      sectionIndexOutputPath (LangCode "en") Posts `shouldBe` "en/posts/index.html"
      sectionIndexOutputPath (LangCode "zh") Slides `shouldBe` "zh/slides/index.html"

    it "renders page output path with empty segments as home output path" $
      pageOutputPath (LangCode "en") [] `shouldBe` "en/index.html"

    it "renders section page identifiers" $ do
      toFilePath (sectionPageIdentifier (LangCode "en") Posts 1) `shouldBe` "en/posts/index.html"
      toFilePath (sectionPageIdentifier (LangCode "en") Posts 2) `shouldBe` "en/posts/page/2/index.html"
      toFilePath (sectionPageIdentifier (LangCode "zh") Slides 5) `shouldBe` "zh/slides/page/5/index.html"

    it "renders content asset output paths (lang first)" $ do
      contentAssetOutputPath (LangCode "en") Posts (Slug "hello-world") "diagram.png"
        `shouldBe` "en/posts/hello-world/diagram.png"
      contentAssetOutputPath (LangCode "zh") Slides (Slug "nix-intro") "cover.jpg"
        `shouldBe` "zh/slides/nix-intro/cover.jpg"

  describe "identifier parsing" $ do
    it "extracts language from localized index files" $ do
      langFromIndexIdentifier (fromFilePath "content/posts/hello-world/index.en.md") `shouldBe` Right (LangCode "en")
      langFromIndexIdentifier (fromFilePath "content/about/index.zh.md") `shouldBe` Right (LangCode "zh")

    it "rejects files that are not localized index markdown" $
      langFromIndexIdentifier (fromFilePath "content/slides/nix-intro/slides.md") `shouldSatisfy` isLeft

    it "extracts post slugs" $
      postSlugFromIdentifier (fromFilePath "content/posts/hello-world/index.zh.md") `shouldBe` Right (Slug "hello-world")

    it "extracts slide slugs" $
      slideSlugFromIdentifier (fromFilePath "content/slides/nix-intro/slides.md") `shouldBe` Right (Slug "nix-intro")

    it "rejects malformed post identifiers" $
      postSlugFromIdentifier (fromFilePath "content/posts/index.zh.md") `shouldSatisfy` isLeft

    it "rejects slide identifier used for post section" $
      postSlugFromIdentifier (fromFilePath "content/slides/nix-intro/slides.md") `shouldSatisfy` isLeft

    it "rejects identifiers not under content/ prefix" $
      pageSegmentsFromIdentifier (fromFilePath "src/Site.hs") `shouldSatisfy` isLeft

    it "extracts standalone page path segments" $ do
      pageSegmentsFromIdentifier (fromFilePath "content/about/index.en.md") `shouldBe` Right ["about"]
      pageSegmentsFromIdentifier (fromFilePath "content/icpc/templates/math/index.zh.md") `shouldBe` Right ["icpc", "templates", "math"]

    it "extracts empty segments for top-level content index" $
      pageSegmentsFromIdentifier (fromFilePath "content/index.en.md") `shouldBe` Right []
