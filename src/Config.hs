{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( SiteConfig (..),
    SiteInfo (..),
    LicenseConfig (..),
    AuthorConfig (..),
    NavItem (..),
    SocialLink (..),
    FriendLink (..),
    FeedConfig (..),
    PaginationConfig (..),
    PaginationItemConfig (..),
    Language (..),
    Translated (..),
    TranslatedList (..),
    loadConfig,
    getTrans,
    getTransList,
    defaultLang,
    langCodes,
    postsPerPage,
    slidesPerPage,
  )
where

import Data.Aeson
import Data.Aeson.Key (toText)
import Data.Aeson.KeyMap (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (decodeFileThrow)
import GHC.Generics (Generic)

newtype Translated = Translated {unTranslated :: Map Text Text}
  deriving (Show)

instance FromJSON Translated where
  parseJSON = withObject "Translated" $ \v -> do
    pairs <- mapM parsePair (toList v)
    pure $ Translated $ Map.fromList pairs
    where
      parsePair (k, val) = do
        t <- parseJSON val
        pure (toText k, t)

newtype TranslatedList = TranslatedList {unTranslatedList :: Map Text [Text]}
  deriving (Show)

instance FromJSON TranslatedList where
  parseJSON = withObject "TranslatedList" $ \v -> do
    pairs <- mapM parsePair (toList v)
    pure $ TranslatedList $ Map.fromList pairs
    where
      parsePair (k, val) = do
        ts <- parseJSON val
        pure (toText k, ts)

getTrans :: [Language] -> String -> Translated -> Text
getTrans langs lang (Translated m) =
  let langText = T.pack lang
      defLang = defaultLang langs
   in case Map.lookup langText m of
        Just t -> t
        Nothing -> Map.findWithDefault "" defLang m

getTransList :: [Language] -> String -> TranslatedList -> [Text]
getTransList langs lang (TranslatedList m) =
  let langText = T.pack lang
      defLang = defaultLang langs
   in case Map.lookup langText m of
        Just ts -> ts
        Nothing -> Map.findWithDefault [] defLang m

defaultLang :: [Language] -> Text
defaultLang [] = "en"
defaultLang (l : _) = langCode l

langCodes :: SiteConfig -> [Text]
langCodes = map langCode . languages

data SiteConfig = SiteConfig
  { site :: SiteInfo,
    languages :: [Language],
    author :: AuthorConfig,
    navigation :: [NavItem],
    social :: [SocialLink],
    friends :: [FriendLink],
    feed :: FeedConfig,
    pagination :: Maybe PaginationConfig
  }
  deriving (Show, Generic)

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
    typewriterPhrases :: TranslatedList,
    baseUrl :: Text,
    copyright :: Translated,
    license :: Maybe LicenseConfig
  }
  deriving (Show, Generic)

data LicenseConfig = LicenseConfig
  { licenseName :: Text,
    licenseUrl :: Text
  }
  deriving (Show, Generic)

instance FromJSON LicenseConfig where
  parseJSON = withObject "LicenseConfig" $ \v ->
    LicenseConfig <$> v .: "name" <*> v .: "url"

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

data FriendLink = FriendLink
  { friendName :: Text,
    friendUrl :: Text
  }
  deriving (Show, Generic)

instance FromJSON FriendLink where
  parseJSON = withObject "FriendLink" $ \v ->
    FriendLink <$> v .: "name" <*> v .: "url"

data FeedConfig = FeedConfig
  { feedTitle :: Translated,
    feedDescription :: Translated,
    feedItemsCount :: Int
  }
  deriving (Show, Generic)

instance FromJSON FeedConfig where
  parseJSON = withObject "FeedConfig" $ \v ->
    FeedConfig <$> v .: "title" <*> v .: "description" <*> v .: "itemsCount"

-- | Pagination configuration for a single item type
data PaginationItemConfig = PaginationItemConfig
  { itemsPerPage :: Int
  }
  deriving (Show, Generic)

instance FromJSON PaginationItemConfig where
  parseJSON = withObject "PaginationItemConfig" $ \v ->
    PaginationItemConfig <$> v .:? "itemsPerPage" .!= 10

-- | Pagination configuration for all item types
data PaginationConfig = PaginationConfig
  { postsPagination :: PaginationItemConfig,
    slidesPagination :: PaginationItemConfig
  }
  deriving (Show, Generic)

instance FromJSON PaginationConfig where
  parseJSON = withObject "PaginationConfig" $ \v ->
    PaginationConfig
      <$> v .:? "posts" .!= PaginationItemConfig 10
      <*> v .:? "slides" .!= PaginationItemConfig 12

-- | Get posts per page from config, with default fallback
postsPerPage :: SiteConfig -> Int
postsPerPage cfg = maybe 10 (itemsPerPage . postsPagination) (pagination cfg)

-- | Get slides per page from config, with default fallback
slidesPerPage :: SiteConfig -> Int
slidesPerPage cfg = maybe 12 (itemsPerPage . slidesPagination) (pagination cfg)

instance FromJSON SiteConfig

instance FromJSON SiteInfo

instance FromJSON AuthorConfig

loadConfig :: FilePath -> IO SiteConfig
loadConfig = decodeFileThrow
