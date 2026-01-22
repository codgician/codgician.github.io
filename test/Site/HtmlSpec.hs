{-# LANGUAGE OverloadedStrings #-}

module Site.HtmlSpec (spec) where

import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Hedgehog (annotate, assert, (===))
import Site.Helpers
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)
import Text.HTML.TagSoup (parseTags)

-- | Site directory to test against
siteDir :: FilePath
siteDir = "_site"

spec :: Spec
spec = describe "Generated HTML" $ do
  -- Page-type invariants
  describe "Pagination" $ do
    it "all paginated list pages have pagination nav" $ hedgehog $ do
      forM_ ["en", "zh"] $ \lang -> do
        forM_ ["posts", "slides"] $ \section -> do
          -- Check page 1
          let page1 = siteDir </> lang </> section </> "index.html"
          page1Exists <- liftIO $ doesFileExist page1
          when page1Exists $ do
            annotate $ "Checking: " <> page1
            content <- liftIO $ TIO.readFile page1
            let tags = parseTags content
            -- Page 1 should have pagination if there are multiple pages
            let page2 = siteDir </> lang </> section </> "page" </> "2" </> "index.html"
            page2Exists <- liftIO $ doesFileExist page2
            when page2Exists $ do
              assert $ hasElementWithClass "nav" "pagination" tags

          -- Check subsequent pages
          forM_ ([2 .. 5] :: [Int]) $ \n -> do
            let pageN = siteDir </> lang </> section </> "page" </> show n </> "index.html"
            pageNExists <- liftIO $ doesFileExist pageN
            when pageNExists $ do
              annotate $ "Checking: " <> pageN
              content <- liftIO $ TIO.readFile pageN
              let tags = parseTags content
              assert $ hasElementWithClass "nav" "pagination" tags

    it "page 2+ has previous link" $ hedgehog $ do
      forM_ ["en", "zh"] $ \lang -> do
        forM_ ["posts", "slides"] $ \section -> do
          forM_ ([2 .. 5] :: [Int]) $ \n -> do
            let pageN = siteDir </> lang </> section </> "page" </> show n </> "index.html"
            pageNExists <- liftIO $ doesFileExist pageN
            when pageNExists $ do
              annotate $ "Checking prev link: " <> pageN
              content <- liftIO $ TIO.readFile pageN
              let tags = parseTags content
              -- Must have an <a> with pagination-prev class (not just disabled span)
              assert $ hasElementWithClass "a" "pagination-prev" tags

    it "page 1 has no previous link as anchor" $ hedgehog $ do
      forM_ ["en", "zh"] $ \lang -> do
        forM_ ["posts", "slides"] $ \section -> do
          let page1 = siteDir </> lang </> section </> "index.html"
          page1Exists <- liftIO $ doesFileExist page1
          when page1Exists $ do
            -- Only check if pagination exists (multi-page)
            let page2 = siteDir </> lang </> section </> "page" </> "2" </> "index.html"
            page2Exists <- liftIO $ doesFileExist page2
            when page2Exists $ do
              annotate $ "Checking no prev anchor: " <> page1
              content <- liftIO $ TIO.readFile page1
              let tags = parseTags content
              -- Should NOT have <a class="pagination-prev">
              assert $ not $ hasElementWithClass "a" "pagination-prev" tags

  -- Cross-cutting invariants
  describe "Navigation" $ do
    it "all pages have nav element (except slide presentations and special pages)" $ hedgehog $ do
      htmlFiles <- liftIO $ listHtmlFiles siteDir
      assert $ not (null htmlFiles) -- Site was built
      forM_ htmlFiles $ \path -> do
        unless (isSlidePresentation path || isSpecialPage path) $ do
          annotate $ "Checking nav: " <> path
          content <- liftIO $ TIO.readFile path
          let tags = parseTags content
          assert $ hasElementWithClass "nav" "nav" tags

  describe "Bilingual" $ do
    it "same post slugs accessible in both languages" $ hedgehog $ do
      enPostDirs <- liftIO $ listDirectories (siteDir </> "en" </> "posts")
      zhPostDirs <- liftIO $ listDirectories (siteDir </> "zh" </> "posts")

      -- Filter out "page" directory (pagination)
      let enSlugs = Set.fromList $ filter (/= "page") enPostDirs
      let zhSlugs = Set.fromList $ filter (/= "page") zhPostDirs

      annotate $ "EN slugs: " <> show (Set.toList enSlugs)
      annotate $ "ZH slugs: " <> show (Set.toList zhSlugs)

      -- Both languages should have the same slugs
      enSlugs === zhSlugs

  describe "RSS Feeds" $ do
    it "feeds have entry elements" $ hedgehog $ do
      forM_ ["en", "zh"] $ \lang -> do
        let feedPath = siteDir </> lang </> "feed.xml"
        feedExists <- liftIO $ doesFileExist feedPath
        assert feedExists
        annotate $ "Checking feed: " <> feedPath
        content <- liftIO $ TIO.readFile feedPath
        -- Basic Atom validity - has entry elements
        assert $ "<entry>" `T.isInfixOf` content
        -- Count entries (should be > 0)
        let entryCount = length $ T.splitOn "<entry>" content
        annotate $ "Entry count: " <> show (entryCount - 1)
        assert $ entryCount > 1 -- splitOn gives n+1 parts for n occurrences
