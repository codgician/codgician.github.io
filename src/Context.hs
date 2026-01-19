{-# LANGUAGE OverloadedStrings #-}

module Context
  ( siteCtx,
    postCtx,
    langCtx,
  )
where

import Config
import qualified Data.Text as T
import Hakyll

-- | Base site context with config values
siteCtx :: SiteConfig -> Context String
siteCtx cfg =
  constField "siteTitle" (T.unpack $ title $ site cfg)
    <> constField "siteSubtitle" (T.unpack $ subtitle $ site cfg)
    <> constField "copyright" (T.unpack $ Config.copyright $ site cfg)
    <> constField "authorName" (T.unpack $ Config.name $ author cfg)
    <> listField "navigation" navItemCtx (pure $ map mkNavItem $ navigation cfg)
    <> listField "social" socialLinkCtx (pure $ map mkSocialItem $ social cfg)
    <> defaultContext
  where
    navItemCtx = field "name" (pure . navItemName) <> field "url" (pure . navItemUrl)
    navItemName = T.unpack . navName . itemBody
    navItemUrl = T.unpack . navUrl . itemBody
    mkNavItem n = Item (fromFilePath "") n

    socialLinkCtx =
      field "name" (pure . sName)
        <> field "url" (pure . sUrl)
        <> field "icon" (pure . sIcon)
    sName = T.unpack . socialName . itemBody
    sUrl = T.unpack . socialUrl . itemBody
    sIcon = T.unpack . socialIcon . itemBody
    mkSocialItem s = Item (fromFilePath "") s

-- | Language context
langCtx :: String -> Context String
langCtx lang =
  constField "lang" lang
    <> constField "langSwitchUrl" (switchUrl lang)
    <> constField "langSwitchLabel" (switchLabel lang)
  where
    switchUrl "en" = "/zh/"
    switchUrl _ = "/en/"
    switchLabel "en" = "中文"
    switchLabel _ = "English"

-- | Post context with date and reading time
postCtx :: SiteConfig -> String -> Context String
postCtx cfg lang =
  dateField "date" "%B %e, %Y"
    <> field "readingTime" readingTime
    <> langCtx lang
    <> siteCtx cfg

-- | Calculate reading time from body
readingTime :: Item String -> Compiler String
readingTime item = do
  let wordCount = length $ words $ itemBody item
      minutes = max 1 (wordCount `div` 200)
  pure $ show minutes <> " min read"
