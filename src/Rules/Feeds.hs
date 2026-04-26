{-# LANGUAGE OverloadedStrings #-}

-- | Rules for RSS/Atom feed generation.
--
-- Creates one Atom feed per configured language at /{lang}/feed.xml.
module Rules.Feeds
  ( feedRules,
  )
where

import Config (FeedConfig (..), Language (..), SiteConfig (..))
import Control.Monad (forM_)
import Feed (feedConfiguration, feedCtxForLang)
import Hakyll
import Routes (feedOutputPath)
import Rules.Posts (loadAllPostsForLang)

-- ============================================================================
-- Public API
-- ============================================================================

-- | RSS/Atom feed rules: one feed per configured language.
feedRules :: SiteConfig -> Rules ()
feedRules cfg = do
  postDep <- makePatternDependency "content/posts/*/index.*.md"
  rulesExtraDependencies [postDep] $
    forM_ (languages cfg) $ \lang -> do
      let lc = langCode lang
      create [fromFilePath $ feedOutputPath lc] $ do
        route idRoute
        compile $ do
          posts <- take (feedItemsCount $ feed cfg) <$> loadAllPostsForLang cfg lc
          let emptyBodyPosts = map (itemSetBody "") posts
          renderAtom (feedConfiguration cfg lc) (feedCtxForLang lc) emptyBodyPosts
