{-# LANGUAGE OverloadedStrings #-}

module Site (hakyllMain) where

import Config
import Hakyll
import Rules.Feeds (feedRules)
import Rules.Pages (pageRules)
import Rules.Posts (postListRules, postRules)
import Rules.Sitemap (sitemapRules)
import Rules.Slides (slideRules)
import Rules.Static (contentAssetRules, staticRules)

-- ============================================================================
-- Constants & Configuration
-- ============================================================================

config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "_site",
      storeDirectory = "_cache",
      tmpDirectory = "_cache/tmp",
      providerDirectory = "."
    }

-- ============================================================================
-- Main Entry Point
-- ============================================================================

hakyllMain :: IO ()
hakyllMain = hakyllWith config rules

rules :: Rules ()
rules = do
  cfg <- preprocess $ loadConfig "config.yaml"
  configDep <- makePatternDependency "config.yaml"

  -- Static files (no config dependency needed)
  staticRules

  -- Content rules (depend on config)
  rulesExtraDependencies [configDep] $ do
    contentAssetRules cfg
    pageRules cfg
    postRules cfg
    postListRules cfg
    feedRules cfg
    sitemapRules cfg
    slideRules cfg
