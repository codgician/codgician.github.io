{-# LANGUAGE OverloadedStrings #-}

-- | Context builders for Hakyll templates.
--
-- Design principles:
-- 1. Composable: Small functions that combine via (<>)
-- 2. Minimal: Each context includes only what's needed
-- 3. Extensible: Easy to add new contexts without modifying existing ones
--
-- Usage:
--   baseCtx cfg lang <> postMetaCtx        -- for blog posts
--   baseCtx cfg lang <> friendsCtx cfg     -- for about page
--   homeCtx cfg lang                       -- for homepage (standalone)
module Context
  ( -- * Core contexts
    baseCtx,

    -- * Page-specific contexts
    homeCtx,
    postMetaCtx,
    dateCtx,
    tocCtx,
    postTagsField,
    tagsFieldFromMeta,

    -- * List contexts
    friendsCtx,
    availableLangsCtx,
    yearGroupsCtx,

    -- * Utilities
    toItem,

    -- * Types
    AvailableLang (..),
    YearGroup (..),
  )
where

import Config
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Hakyll

-- ============================================================================
-- Types
-- ============================================================================

-- | Language option for language switcher
data AvailableLang = AvailableLang
  { alCode :: String,
    alLabel :: String,
    alUrl :: String,
    alActive :: Bool
  }

-- | Year group for post archives
data YearGroup = YearGroup
  { ygYear :: String,
    ygPosts :: [Item String]
  }

-- ============================================================================
-- Core Contexts
-- ============================================================================

-- | Minimal base context with essential site-wide fields.
-- Includes: siteTitle, copyright, license, authorName, lang, navigation, defaultContext
baseCtx :: SiteConfig -> String -> Context String
baseCtx cfg lang =
  constField "siteTitle" (trans $ title $ site cfg)
    <> constField "copyright" (trans $ Config.copyright $ site cfg)
    <> licenseCtx (license $ site cfg)
    <> constField "authorName" (T.unpack $ Config.name $ author cfg)
    <> langCtx lang
    <> navCtx cfg lang
    <> defaultContext
  where
    trans = transStr (languages cfg) lang

-- | Language field context
langCtx :: String -> Context String
langCtx = constField "lang"

-- ============================================================================
-- Page-Specific Contexts
-- ============================================================================

-- | Homepage context with social links and typewriter effect.
-- Composes baseCtx with homepage-specific fields.
homeCtx :: SiteConfig -> String -> Context String
homeCtx cfg lang =
  constField "siteSubtitle" (trans $ subtitle $ site cfg)
    <> constField "typewriterPhrases" (phrasesStr $ typewriterPhrases $ site cfg)
    <> socialCtx cfg lang
    <> baseCtx cfg lang
  where
    trans = transStr langs lang
    langs = languages cfg
    phrasesStr = intercalate "|" . map T.unpack . getTransList langs lang

-- | License context (optional)
licenseCtx :: Maybe LicenseConfig -> Context String
licenseCtx Nothing = mempty
licenseCtx (Just lic) =
  constField "licenseName" (T.unpack $ licenseName lic)
    <> constField "licenseUrl" (T.unpack $ licenseUrl lic)

-- | Post metadata context (date fields, reading time)
-- Compose with baseCtx: baseCtx cfg lang <> postMetaCtx
postMetaCtx :: Context String
postMetaCtx =
  dateCtx
    <> field "readingTime" calcReadingTime
  where
    calcReadingTime item =
      -- stripTags from Hakyll.Web.Html removes HTML tags
      let plainText = stripTags $ itemBody item
          wordCount = length $ words plainText
          minutes = max 1 (wordCount `div` 200)
       in pure $ show minutes <> " min read"

-- | Common date fields used by posts and list items
dateCtx :: Context String
dateCtx =
  dateField "date" "%B %e, %Y"
    <> dateField "dateShort" "%b %d"
    <> dateField "dateYear" "%Y"

-- | Table of contents context (optional)
-- Pass the TOC HTML and language; provides toc, hasToc, and tocTitle fields
tocCtx :: String -> Maybe String -> Context String
tocCtx _ Nothing = mempty
tocCtx lang (Just tocHtml) =
  constField "toc" tocHtml
    <> constField "hasToc" "true"
    <> constField "tocTitle" (tocTitleForLang lang)

-- | Get localized TOC title
tocTitleForLang :: String -> String
tocTitleForLang "zh" = "目录"
tocTitleForLang _ = "Table of Contents"

-- | Tags field that reads from metadata and provides a list for iteration
-- Usage in templates: $if(tags)$...$for(tags)$<a href="...">$tag$</a>$endfor$...$endif$
-- Only provides the field when tags are present (so $if(tags)$ works correctly)
postTagsField :: String -> Context String
postTagsField lang = Context $ \key _ item -> do
  meta <- getMetadata (itemIdentifier item)
  let tagList = fromMaybe [] $ lookupStringList "tags" meta
  if key == "tags" && not (null tagList)
    then do
      let tagCtx = field "tag" (pure . itemBody) <> constField "lang" lang
      unContext (listField "tags" tagCtx (pure $ map toItem tagList)) key [] item
    else noResult $ "No field " ++ key

-- | Tags field from explicit metadata (for fallback posts)
tagsFieldFromMeta :: String -> Metadata -> Context String
tagsFieldFromMeta lang meta =
  case lookupStringList "tags" meta of
    Just tagList@(_ : _) ->
      let tagCtx = field "tag" (pure . itemBody) <> constField "lang" lang
       in listField "tags" tagCtx (pure $ map toItem tagList)
    _ -> mempty

-- ============================================================================
-- List Contexts
-- ============================================================================

-- | Navigation links context
navCtx :: SiteConfig -> String -> Context String
navCtx cfg lang =
  listField "navigation" itemCtx (pure $ map toItem $ navigation cfg)
  where
    itemCtx =
      field "name" (pure . trans . navLabel . itemBody)
        <> field "url" (pure . T.unpack . navUrl . itemBody)
        <> constField "lang" lang -- Include lang in each nav item for template access
    trans = transStr (languages cfg) lang

-- | Social links context
socialCtx :: SiteConfig -> String -> Context String
socialCtx cfg lang =
  listField "social" itemCtx (pure $ map toItem $ social cfg)
  where
    itemCtx =
      field "name" (pure . trans . socialLabel . itemBody)
        <> field "url" (pure . T.unpack . socialUrl . itemBody)
        <> field "icon" (pure . T.unpack . socialIcon . itemBody)

    trans = transStr (languages cfg) lang

-- | Friends list context
friendsCtx :: SiteConfig -> Context String
friendsCtx cfg =
  listField "friends" itemCtx (pure $ map toItem $ friends cfg)
  where
    itemCtx =
      field "name" (pure . T.unpack . friendName . itemBody)
        <> field "url" (pure . T.unpack . friendUrl . itemBody)

-- | Available languages context for language switcher
availableLangsCtx :: [AvailableLang] -> Context String
availableLangsCtx langs =
  listField "availableLangs" itemCtx (pure $ map toItem langs)
    <> boolField "hasMultipleLangs" (const $ length langs > 1)
  where
    itemCtx =
      field "code" (pure . alCode . itemBody)
        <> field "label" (pure . alLabel . itemBody)
        <> field "url" (pure . alUrl . itemBody)
        <> boolField "active" (alActive . itemBody)

-- | Year groups context for post archives
yearGroupsCtx :: Context String -> Context YearGroup
yearGroupsCtx postCtx' =
  field "year" (pure . ygYear . itemBody)
    <> listFieldWith "posts" postCtx' (pure . ygPosts . itemBody)

-- ============================================================================
-- Utilities
-- ============================================================================

-- | Convert a value to an Item (for list fields with non-Item data)
toItem :: a -> Item a
toItem = Item (fromFilePath "")
