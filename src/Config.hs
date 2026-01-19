{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( SiteConfig (..),
    SiteInfo (..),
    AuthorConfig (..),
    NavItem (..),
    SocialLink (..),
    FeedConfig (..),
    Language (..),
    Translated (..),
    loadConfig,
    getTrans,
  )
where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (decodeFileThrow)
import GHC.Generics (Generic)

-- | Translated text with per-language values
data Translated = Translated
  { en :: Text,
    zh :: Text
  }
  deriving (Show, Generic)

instance FromJSON Translated

-- | Get text for a specific language (falls back to English)
getTrans :: String -> Translated -> Text
getTrans "zh" t = zh t
getTrans _ t = en t

data SiteConfig = SiteConfig
  { site :: SiteInfo,
    languages :: [Language],
    author :: AuthorConfig,
    navigation :: [NavItem],
    social :: [SocialLink],
    feed :: FeedConfig
  }
  deriving (Show, Generic)

-- | Language definition
data Language = Language
  { langCode :: Text,
    langLabel :: Text
  }
  deriving (Show, Generic)

instance FromJSON Language where
  parseJSON = withObject "Language" $ \v ->
    Language <$> v .: "code" <*> v .: "label"

data SiteInfo = SiteInfo
  { title :: Translated,
    subtitle :: Translated,
    baseUrl :: Text,
    copyright :: Translated
  }
  deriving (Show, Generic)

data AuthorConfig = AuthorConfig
  { name :: Text,
    email :: Text
  }
  deriving (Show, Generic)

data NavItem = NavItem
  { navLabel :: Translated,
    navUrl :: Text
  }
  deriving (Show, Generic)

instance FromJSON NavItem where
  parseJSON = withObject "NavItem" $ \v ->
    NavItem <$> v .: "label" <*> v .: "url"

data SocialLink = SocialLink
  { socialLabel :: Translated,
    socialUrl :: Text,
    socialIcon :: Text
  }
  deriving (Show, Generic)

instance FromJSON SocialLink where
  parseJSON = withObject "SocialLink" $ \v ->
    SocialLink <$> v .: "label" <*> v .: "url" <*> v .: "icon"

data FeedConfig = FeedConfig
  { feedTitle :: Translated,
    feedDescription :: Translated,
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
