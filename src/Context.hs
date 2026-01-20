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
    langCtx,

    -- * Page-specific contexts
    homeCtx,
    postMetaCtx,

    -- * List contexts
    navCtx,
    socialCtx,
    friendsCtx,
    availableLangsCtx,
    yearGroupsCtx,

    -- * Types
    AvailableLang (..),
    YearGroup (..),
  )
where

import Config
import Data.List (intercalate)
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
    trans = T.unpack . getTrans (languages cfg) lang

-- | Language field context
langCtx :: String -> Context String
langCtx = constField "lang"

-- ============================================================================
-- Page-Specific Contexts
-- ============================================================================

-- | Homepage context with social links and typewriter effect.
-- Standalone context (includes baseCtx internally for different structure)
homeCtx :: SiteConfig -> String -> Context String
homeCtx cfg lang =
  constField "siteTitle" (trans $ title $ site cfg)
    <> constField "siteSubtitle" (trans $ subtitle $ site cfg)
    <> constField "typewriterPhrases" (phrasesStr $ typewriterPhrases $ site cfg)
    <> constField "copyright" (trans $ Config.copyright $ site cfg)
    <> licenseCtx (license $ site cfg)
    <> langCtx lang
    <> navCtx cfg lang
    <> socialCtx cfg lang
    <> defaultContext
  where
    trans = T.unpack . getTrans langs lang
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
  dateField "date" "%B %e, %Y"
    <> dateField "dateShort" "%b %d"
    <> dateField "dateYear" "%Y"
    <> field "readingTime" calcReadingTime
  where
    calcReadingTime item =
      let wordCount = length $ words $ itemBody item
          minutes = max 1 (wordCount `div` 200)
       in pure $ show minutes <> " min read"

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
        <> constField "lang" lang  -- Include lang in each nav item for template access

    trans = T.unpack . getTrans (languages cfg) lang
    toItem = Item (fromFilePath "")

-- | Social links context
socialCtx :: SiteConfig -> String -> Context String
socialCtx cfg lang =
  listField "social" itemCtx (pure $ map toItem $ social cfg)
  where
    itemCtx =
      field "name" (pure . trans . socialLabel . itemBody)
        <> field "url" (pure . T.unpack . socialUrl . itemBody)
        <> field "icon" (pure . T.unpack . socialIcon . itemBody)

    trans = T.unpack . getTrans (languages cfg) lang
    toItem = Item (fromFilePath "")

-- | Friends list context
friendsCtx :: SiteConfig -> Context String
friendsCtx cfg =
  listField "friends" itemCtx (pure $ map toItem $ friends cfg)
  where
    itemCtx =
      field "name" (pure . T.unpack . friendName . itemBody)
        <> field "url" (pure . T.unpack . friendUrl . itemBody)

    toItem = Item (fromFilePath "")

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

    toItem = Item (fromFilePath "")

-- | Year groups context for post archives
yearGroupsCtx :: Context String -> Context YearGroup
yearGroupsCtx postCtx' =
  field "year" (pure . ygYear . itemBody)
    <> listFieldWith "posts" postCtx' (pure . ygPosts . itemBody)
