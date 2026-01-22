{-# LANGUAGE OverloadedStrings #-}

module PaginateSpec (spec) where

import Hakyll (toFilePath)
import Paginate
import Test.Hspec

spec :: Spec
spec = describe "Paginate" $
  describe "makePageId" $ do
    it "generates clean URL for page 1" $ do
      toFilePath (makePageId "en" "posts" 1) `shouldBe` "en/posts/index.html"
      toFilePath (makePageId "zh" "posts" 1) `shouldBe` "zh/posts/index.html"

    it "generates paginated URL for page 2+" $ do
      toFilePath (makePageId "en" "posts" 2) `shouldBe` "en/posts/page/2/index.html"
      toFilePath (makePageId "en" "posts" 3) `shouldBe` "en/posts/page/3/index.html"
      toFilePath (makePageId "zh" "slides" 5) `shouldBe` "zh/slides/page/5/index.html"

    it "works with different sections" $ do
      toFilePath (makePageId "en" "slides" 1) `shouldBe` "en/slides/index.html"
      toFilePath (makePageId "en" "slides" 2) `shouldBe` "en/slides/page/2/index.html"
