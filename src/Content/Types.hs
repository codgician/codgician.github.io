{-# LANGUAGE OverloadedStrings #-}

module Content.Types
  ( LangCode (..),
    Slug (..),
    Section (..),
    ContentRef (..),
    LocalizedRef (..),
    RenderFeatures (..),
    langCodeString,
    slugString,
    sectionPathSegment,
    sectionPathString,
    sectionFromPathSegment,
    safeInit,
    nubOrd,
  )
where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

newtype LangCode = LangCode {unLangCode :: Text}
  deriving (Eq, Ord, Show)

newtype Slug = Slug {unSlug :: Text}
  deriving (Eq, Ord, Show)

data Section
  = Posts
  | Slides
  deriving (Eq, Ord, Show)

data ContentRef = ContentRef
  { contentSection :: Section,
    contentSlug :: Slug
  }
  deriving (Eq, Ord, Show)

data LocalizedRef = LocalizedRef
  { localizedLang :: LangCode,
    localizedContent :: ContentRef
  }
  deriving (Eq, Ord, Show)

data RenderFeatures = RenderFeatures
  { renderMath :: Bool,
    renderMermaid :: Bool,
    renderTikZ :: Bool,
    renderToc :: Bool
  }
  deriving (Eq, Show)

langCodeString :: LangCode -> String
langCodeString = T.unpack . unLangCode

slugString :: Slug -> String
slugString = T.unpack . unSlug

sectionPathSegment :: Section -> Text
sectionPathSegment Posts = "posts"
sectionPathSegment Slides = "slides"

sectionPathString :: Section -> String
sectionPathString = T.unpack . sectionPathSegment

sectionFromPathSegment :: Text -> Maybe Section
sectionFromPathSegment "posts" = Just Posts
sectionFromPathSegment "slides" = Just Slides
sectionFromPathSegment _ = Nothing

-- | Safe init: returns empty list instead of error on empty input.
safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

-- | O(n log n) deduplication preserving one copy of each element.
nubOrd :: (Ord a) => [a] -> [a]
nubOrd = Set.toList . Set.fromList
