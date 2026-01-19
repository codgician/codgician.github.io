{-# LANGUAGE OverloadedStrings #-}

module Context
  ( siteCtx,
    postCtx,
    langCtx,
    availableLangsCtx,
    AvailableLang (..),
  )
where

import Config
import qualified Data.Text as T
import Hakyll

-- | Available language for a page
data AvailableLang = AvailableLang
  { alCode :: String,
    alLabel :: String,
    alUrl :: String,
    alActive :: Bool
  }

-- | Base site context with config values (language-aware)
siteCtx :: SiteConfig -> String -> Context String
siteCtx cfg lang =
  constField "siteTitle" (T.unpack $ getTrans lang $ title $ site cfg)
    <> constField "siteSubtitle" (T.unpack $ getTrans lang $ subtitle $ site cfg)
    <> constField "copyright" (T.unpack $ getTrans lang $ Config.copyright $ site cfg)
    <> constField "authorName" (T.unpack $ Config.name $ author cfg)
    <> listField "navigation" (navItemCtx lang) (pure $ map mkNavItem $ navigation cfg)
    <> listField "social" (socialLinkCtx lang) (pure $ map mkSocialItem $ social cfg)
    <> defaultContext
  where
    navItemCtx l =
      field "name" (\item -> pure $ T.unpack $ getTrans l $ navLabel $ itemBody item)
        <> field "url" (pure . T.unpack . navUrl . itemBody)
        <> constField "lang" l
    mkNavItem n = Item (fromFilePath "") n

    socialLinkCtx l =
      field "name" (\item -> pure $ T.unpack $ getTrans l $ socialLabel $ itemBody item)
        <> field "url" (pure . T.unpack . socialUrl . itemBody)
        <> field "icon" (pure . T.unpack . socialIcon . itemBody)
    mkSocialItem s = Item (fromFilePath "") s

-- | Language context (basic lang field)
langCtx :: String -> Context String
langCtx lang = constField "lang" lang

-- | Context for available languages on a page
availableLangsCtx :: [AvailableLang] -> Context String
availableLangsCtx langs =
  listField "availableLangs" langItemCtx (pure $ map mkLangItem langs)
    <> boolField "hasMultipleLangs" (const $ length langs > 1)
  where
    langItemCtx =
      field "code" (pure . alCode . itemBody)
        <> field "label" (pure . alLabel . itemBody)
        <> field "url" (pure . alUrl . itemBody)
        <> boolField "active" (alActive . itemBody)
    mkLangItem l = Item (fromFilePath "") l

-- | Post context with date and reading time
postCtx :: SiteConfig -> String -> Context String
postCtx cfg lang =
  dateField "date" "%B %e, %Y"
    <> dateField "dateShort" "%b %d"
    <> field "readingTime" readingTime
    <> langCtx lang
    <> siteCtx cfg lang

-- | Calculate reading time from body
readingTime :: Item String -> Compiler String
readingTime item = do
  let wordCount = length $ words $ itemBody item
      minutes = max 1 (wordCount `div` 200)
  pure $ show minutes <> " min read"
