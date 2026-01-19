{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( SiteConfig (..),
    SiteInfo (..),
    AuthorConfig (..),
    NavItem (..),
    SocialLink (..),
    FeedConfig (..),
    loadConfig,
  )
where

import Data.Aeson
import Data.Text (Text)
import Data.Yaml (decodeFileThrow)
import GHC.Generics (Generic)

data SiteConfig = SiteConfig
  { site :: SiteInfo,
    author :: AuthorConfig,
    navigation :: [NavItem],
    social :: [SocialLink],
    feed :: FeedConfig
  }
  deriving (Show, Generic)

data SiteInfo = SiteInfo
  { title :: Text,
    subtitle :: Text,
    baseUrl :: Text,
    copyright :: Text,
    defaultLanguage :: Text,
    supportedLanguages :: [Text]
  }
  deriving (Show, Generic)

data AuthorConfig = AuthorConfig
  { name :: Text,
    email :: Text
  }
  deriving (Show, Generic)

data NavItem = NavItem
  { navName :: Text,
    navUrl :: Text
  }
  deriving (Show, Generic)

instance FromJSON NavItem where
  parseJSON = withObject "NavItem" $ \v ->
    NavItem <$> v .: "name" <*> v .: "url"

data SocialLink = SocialLink
  { socialName :: Text,
    socialUrl :: Text,
    socialIcon :: Text
  }
  deriving (Show, Generic)

instance FromJSON SocialLink where
  parseJSON = withObject "SocialLink" $ \v ->
    SocialLink <$> v .: "name" <*> v .: "url" <*> v .: "icon"

data FeedConfig = FeedConfig
  { feedTitle :: Text,
    feedDescription :: Text,
    feedItemsCount :: Int
  }
  deriving (Show, Generic)

instance FromJSON FeedConfig where
  parseJSON = withObject "FeedConfig" $ \v ->
    FeedConfig <$> v .: "title" <*> v .: "description" <*> v .: "itemsCount"

instance FromJSON SiteConfig
instance FromJSON SiteInfo
instance FromJSON AuthorConfig

loadConfig :: FilePath -> IO SiteConfig
loadConfig = decodeFileThrow
