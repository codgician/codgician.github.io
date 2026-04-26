{-# LANGUAGE OverloadedStrings #-}

-- | Rules for Reveal.js slide pages and slide list pagination.
--
-- Handles:
-- - Individual slide compilation (one version per language)
-- - Paginated slide list per language
module Rules.Slides
  ( slideRules,
  )
where

import Compiler.Pandoc (slideCompiler)
import Config (Language (..), SiteConfig (..), slidesPerPage)
import Content.Metadata (metadataBool)
import Content.Types (LangCode, Section (..), langCodeString)
import Context
  ( allLangsCtx,
    baseCtx,
    navTitle,
  )
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Hakyll
import Paginate (makePageId, paginationCtx)
import Routes
  ( routeErrorMessage,
    routeOrFail,
    sectionIndexUrl,
    slideOutputPath,
    slideSlugFromIdentifier,
    slideUrl,
  )

-- ============================================================================
-- Public API
-- ============================================================================

-- | All slide rules: individual slide pages and the paginated slide list.
slideRules :: SiteConfig -> Rules ()
slideRules cfg = do
  slidePages cfg
  slideList cfg

-- ============================================================================
-- Individual slide pages
-- ============================================================================

slidePages :: SiteConfig -> Rules ()
slidePages cfg = match "content/slides/*/slides.md" $
  forM_ (languages cfg) $ \lang ->
    version (langCodeString $ langCode lang) $ do
      let lc = langCode lang
      route $ customRoute $ \i ->
        either (error . routeErrorMessage) (slideOutputPath lc) $
          slideSlugFromIdentifier i
      compile $ do
        ident <- getUnderlying
        meta <- getMetadata ident
        let enableMath = metadataBool "math" meta
            enableTikZ = metadataBool "tikz" meta
        slideCompiler enableMath enableTikZ
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/slide.html" defaultContext
          >>= relativizeUrls

-- ============================================================================
-- Slide list (paginated)
-- ============================================================================

slideList :: SiteConfig -> Rules ()
slideList cfg = do
  slideDep <- makePatternDependency "content/slides/*/slides.md"
  rulesExtraDependencies [slideDep] $
    forM_ (languages cfg) $ \lang -> do
      let lc = langCode lang
          ls = langCodeString lc
      paginate <-
        buildPaginateWith
          (fmap (paginateEvery (slidesPerPage cfg)) . sortRecentFirst)
          (fromGlob "content/slides/*/slides.md")
          (makePageId lc Slides)
      paginateRules paginate $ \pageNum _ -> do
        route idRoute
        compile $ do
          slides <- loadAllSnapshots (fromGlob "content/slides/*/slides.md" .&&. hasVersion ls) "content"
          let slideCtx = field "url" (makeSlideUrl lc) <> field "date" getDate <> defaultContext
              ctx =
                constField "title" (navTitle cfg lc (T.pack "slides/"))
                  <> listField "slides" slideCtx (pure slides)
                  <> paginationCtx paginate pageNum
                  <> allLangsCtx cfg lc (`sectionIndexUrl` Slides)
                  <> baseCtx cfg lc
          makeItem ""
            >>= loadAndApplyTemplate "templates/slide-list.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls
  where
    makeSlideUrl :: LangCode -> Item String -> Compiler String
    makeSlideUrl lc item = routeOrFail $ slideUrl lc <$> slideSlugFromIdentifier (itemIdentifier item)

    getDate :: Item String -> Compiler String
    getDate item = fromMaybe "" . lookupString "date" <$> getMetadata (itemIdentifier item)
