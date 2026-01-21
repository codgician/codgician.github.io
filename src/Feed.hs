{-# LANGUAGE OverloadedStrings #-}

module Feed
  ( feedConfiguration,
    feedCtx,
    feedCtxForLang,
  )
where

import qualified Config
import Config (SiteConfig (..))
import qualified Data.Text as T
import Hakyll
import System.FilePath (splitDirectories)

-- | Create feed configuration from site config
feedConfiguration :: SiteConfig -> String -> FeedConfiguration
feedConfiguration cfg lang =
  FeedConfiguration
    { feedTitle = T.unpack $ Config.getTrans langs lang $ Config.feedTitle $ feed cfg,
      feedDescription = T.unpack $ Config.getTrans langs lang $ Config.feedDescription $ feed cfg,
      feedAuthorName = T.unpack $ Config.name $ author cfg,
      feedAuthorEmail = T.unpack $ Config.email $ author cfg,
      feedRoot = T.unpack (Config.baseUrl $ site cfg)
    }
  where
    langs = Config.languages cfg

-- | Feed context (basic, uses item's natural URL)
feedCtx :: Context String
feedCtx =
  bodyField "description"
    <> dateField "date" "%Y-%m-%d"
    <> defaultContext

-- | Feed context for a specific target language
-- Overrides URL to point to the target language path, regardless of source language
feedCtxForLang :: String -> Context String
feedCtxForLang targetLang =
  bodyField "description"
    <> dateField "date" "%Y-%m-%d"
    <> urlFieldForLang targetLang
    <> defaultContext

-- | URL field that generates URL for target language
-- Takes source identifier like "content/posts/{slug}/index.{lang}.md"
-- and generates URL like "/{targetLang}/posts/{slug}/"
urlFieldForLang :: String -> Context a
urlFieldForLang targetLang = field "url" $ \item -> do
  let ident = itemIdentifier item
      path = toFilePath ident
      parts = splitDirectories path
      -- content/posts/{slug}/index.{lang}.md -> parts = ["content", "posts", slug, ...]
      slug = parts !! 2
  pure $ "/" <> targetLang <> "/posts/" <> slug <> "/"
