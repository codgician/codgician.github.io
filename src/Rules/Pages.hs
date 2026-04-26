{-# LANGUAGE OverloadedStrings #-}

-- | Rules for standalone pages (homepage and nested pages).
--
-- Handles:
-- - Homepage per language (content/index.*.md)
-- - Standalone nested pages (content/**/index.*.md)
-- - Language fallback: every page reachable at every configured language URL
-- - Language switcher context for pages
module Rules.Pages
  ( pageRules,
  )
where

import Compiler.Pandoc (customPandocCompiler)
import Config (Language (..), SiteConfig (..), langCodes)
import Content.Fallback (preferredLangOrder)
import Content.Metadata (metadataBool, templateFromMetadata)
import Content.Types (LangCode, langCodeString, nubOrd)
import Context
  ( AvailableLang (..),
    allLangsCtx,
    availableLangsCtx,
    homeCtx,
    pageCtx,
  )
import Control.Monad (forM_, unless)
import Data.Either (rights)
import qualified Data.Text as T
import Hakyll
import Routes
  ( homeOutputPath,
    homeUrl,
    langFromIndexIdentifier,
    pageOutputPath,
    pageSegmentsFromIdentifier,
    pageUrl,
    routeErrorMessage,
    routeOrFail,
  )
import System.FilePath (joinPath, (</>))

-- ============================================================================
-- Public API
-- ============================================================================

-- | All page rules: homepage and standalone nested pages.
pageRules :: SiteConfig -> Rules ()
pageRules cfg = do
  homepages cfg
  standalonePages cfg

-- ============================================================================
-- Homepages
-- ============================================================================

homepages :: SiteConfig -> Rules ()
homepages cfg = match "content/index.*.md" $ do
  route $ customRoute $ \i ->
    either (error . routeErrorMessage) homeOutputPath $
      langFromIndexIdentifier i
  compile $ do
    lang <- routeOrFail . langFromIndexIdentifier =<< getUnderlying
    let ctx = allLangsCtx cfg lang homeUrl <> homeCtx cfg lang
    getResourceBody >>= applyAsTemplate ctx >>= loadAndApplyTemplate "templates/home.html" ctx

-- ============================================================================
-- Standalone nested pages
-- ============================================================================

standalonePages :: SiteConfig -> Rules ()
standalonePages cfg = do
  -- Compile real source pages and save snapshots
  match
    ( "content/**/index.*.md"
        .&&. complement "content/index.*.md"
        .&&. complement "content/posts/**"
        .&&. complement "content/slides/**"
    )
    $ do
      route $ customRoute $ \i ->
        either (error . routeErrorMessage) id $ do
          lang <- langFromIndexIdentifier i
          segs <- pageSegmentsFromIdentifier i
          pure $ pageOutputPath lang segs
      compile $ do
        ident <- getUnderlying
        lang <- routeOrFail $ langFromIndexIdentifier ident
        segments <- routeOrFail $ pageSegmentsFromIdentifier ident
        langsCtx' <- nestedPageLangsCtx cfg lang ident segments
        meta <- getMetadata ident
        let enableMath = metadataBool "math" meta
            enableMermaid = metadataBool "mermaid" meta
            enableTikZ = metadataBool "tikz" meta
            tpl = templateFromMetadata meta
            ctx = langsCtx' <> pageCtx cfg lang tpl
        customPandocCompiler enableMath enableMermaid enableTikZ
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate (fromFilePath $ "templates/" <> tpl <> ".html") ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

  -- Create fallback pages for missing translations
  pageDep <-
    makePatternDependency
      ( "content/**/index.*.md"
          .&&. complement "content/index.*.md"
          .&&. complement "content/posts/**"
          .&&. complement "content/slides/**"
      )
  rulesExtraDependencies [pageDep] $ do
    pageFiles <-
      getMatches
        ( "content/**/index.*.md"
            .&&. complement "content/index.*.md"
            .&&. complement "content/posts/**"
            .&&. complement "content/slides/**"
        )
    let segmentSets = nubOrd $ rights $ map pageSegmentsFromIdentifier pageFiles
    forM_ (langCodes cfg) $ \lang ->
      forM_ segmentSets $ \segs ->
        unless (hasPageTranslation lang segs pageFiles) $
          createFallbackPage cfg lang segs

-- | Check whether a translation already exists for (lang, segments).
hasPageTranslation :: LangCode -> [T.Text] -> [Identifier] -> Bool
hasPageTranslation lang segs =
  any $ \i ->
    langFromIndexIdentifier i == Right lang
      && pageSegmentsFromIdentifier i == Right segs

-- | Create a fallback page for a (lang, segments) pair with no real translation.
createFallbackPage :: SiteConfig -> LangCode -> [T.Text] -> Rules ()
createFallbackPage cfg requestedLang segs =
  create [fromFilePath $ pageOutputPath requestedLang segs] $ do
    route idRoute
    compile $ do
      maybeSource <- findPageIdent cfg requestedLang segs
      case maybeSource of
        Nothing ->
          fail $
            "No source found for page: "
              <> T.unpack (T.intercalate "/" segs)
        Just sourceIdent ->
          renderFallbackPage cfg requestedLang segs sourceIdent

-- | Render a fallback page from a source identifier.
renderFallbackPage :: SiteConfig -> LangCode -> [T.Text] -> Identifier -> Compiler (Item String)
renderFallbackPage cfg requestedLang segs sourceIdent = do
  srcItem <- loadSnapshot sourceIdent "content"
  srcMeta <- getMetadata sourceIdent
  let tpl = templateFromMetadata srcMeta
      langsCtx' = fallbackLangsCtx cfg requestedLang segs sourceIdent
      ctx = langsCtx' <> pageCtx cfg requestedLang tpl
  makeItem (itemBody srcItem)
    >>= loadAndApplyTemplate (fromFilePath $ "templates/" <> tpl <> ".html") ctx
    >>= loadAndApplyTemplate "templates/default.html" ctx
    >>= relativizeUrls

-- | Find the best source identifier for a page given requested lang and segments.
findPageIdent :: SiteConfig -> LangCode -> [T.Text] -> Compiler (Maybe Identifier)
findPageIdent cfg targetLang segs = go (preferredLangOrder cfg targetLang)
  where
    go [] = pure Nothing
    go (l : ls) = do
      let dir = "content" </> joinPath (map T.unpack segs)
          glob = dir </> "index." <> langCodeString l <> ".md"
      found <- getMatches (fromGlob glob)
      case found of
        (i : _) -> pure $ Just i
        [] -> go ls

-- | Build language switcher context for a fallback page.
-- All configured languages are reachable (via real translation or fallback),
-- so all appear in the switcher.
fallbackLangsCtx :: SiteConfig -> LangCode -> [T.Text] -> Identifier -> Context String
fallbackLangsCtx cfg curLang segs _sourceIdent =
  availableLangsCtx
    [ AvailableLang
        (langCodeString $ langCode l)
        (T.unpack $ langLabel l)
        (pageUrl (langCode l) segs)
        (langCode l == curLang)
    | l <- languages cfg
    ]

-- ============================================================================
-- Language switcher for nested pages
-- ============================================================================

-- | Build the language switcher context for a nested page.
--
-- Every configured language URL is reachable because missing translations are
-- rendered from the best available source language.
nestedPageLangsCtx :: SiteConfig -> LangCode -> Identifier -> [T.Text] -> Compiler (Context String)
nestedPageLangsCtx cfg curLang _ident segments = pure $ fallbackLangsCtx cfg curLang segments (fromFilePath "")
