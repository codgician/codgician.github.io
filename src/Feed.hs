{-# LANGUAGE OverloadedStrings #-}

module Feed
  ( feedConfiguration,
    feedCtx,
  )
where

import qualified Config
import Config (SiteConfig (..))
import qualified Data.Text as T
import Hakyll

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

-- | Feed context
feedCtx :: Context String
feedCtx =
  bodyField "description"
    <> dateField "date" "%Y-%m-%d"
    <> defaultContext
