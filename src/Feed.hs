{-# LANGUAGE OverloadedStrings #-}

module Feed
  ( feedConfiguration,
    feedCtxForLang,
  )
where

import Config (SiteConfig (..))
import qualified Config
import Content.Types (LangCode (..))
import qualified Data.Text as T
import Hakyll
import Routes (postSlugFromIdentifier, postUrl, routeOrFail)

-- | Create feed configuration from site config
feedConfiguration :: SiteConfig -> LangCode -> FeedConfiguration
feedConfiguration cfg lang =
  FeedConfiguration
    { feedTitle = T.unpack $ Config.getTrans langs lang $ Config.feedTitle $ feed cfg,
      feedDescription = T.unpack $ Config.getTrans langs lang $ Config.feedDescription $ feed cfg,
      feedAuthorName = T.unpack $ Config.name $ author cfg,
      feedAuthorEmail = maybe "" T.unpack $ Config.email $ author cfg,
      feedRoot = T.unpack (Config.baseUrl $ site cfg)
    }
  where
    langs = Config.languages cfg

-- | Feed context for a specific target language.
-- Overrides URL to point to the target language path, regardless of source language.
feedCtxForLang :: LangCode -> Context String
feedCtxForLang targetLang =
  constField "description" ""
    <> dateField "date" "%Y-%m-%d"
    <> urlFieldForLang targetLang
    <> defaultContext

-- | URL field that generates URL for target language.
-- Takes source identifier like "content/posts/{slug}/index.{lang}.md"
-- and generates URL like "/{targetLang}/posts/{slug}/" using typed route helpers.
urlFieldForLang :: LangCode -> Context a
urlFieldForLang targetLang = field "url" $ \item ->
  routeOrFail $ postUrl targetLang <$> postSlugFromIdentifier (itemIdentifier item)
