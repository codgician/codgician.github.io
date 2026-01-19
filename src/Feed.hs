{-# LANGUAGE OverloadedStrings #-}

module Feed
  ( feedConfiguration,
    feedCtx,
  )
where

import Config (FeedConfig (..), SiteConfig (..))
import qualified Config
import qualified Data.Text as T
import Hakyll

-- | Create feed configuration from site config
feedConfiguration :: SiteConfig -> String -> FeedConfiguration
feedConfiguration cfg lang =
  FeedConfiguration
    { feedTitle = T.unpack $ feedTitle $ feed cfg,
      feedDescription = T.unpack $ feedDescription $ feed cfg,
      feedAuthorName = T.unpack $ Config.name $ author cfg,
      feedAuthorEmail = T.unpack $ Config.email $ author cfg,
      feedRoot = T.unpack (baseUrl $ site cfg) <> "/" <> lang
    }

-- | Feed context
feedCtx :: Context String
feedCtx =
  bodyField "description"
    <> dateField "date" "%Y-%m-%d"
    <> defaultContext
