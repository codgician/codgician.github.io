{-# LANGUAGE OverloadedStrings #-}

-- | Rules for XML sitemap generation.
--
-- Generates sitemap.xml with all public URLs: static pages, posts, and slides.
module Rules.Sitemap
  ( sitemapRules,
  )
where

import Config (Language (..), SiteConfig (..), SiteInfo (..))
import Content.Types (LangCode, Section (..), Slug (..), nubOrd)
import Data.Either (rights)
import qualified Data.Text as T
import Hakyll
import Routes
  ( homeUrl,
    pageSegmentsFromIdentifier,
    pageUrl,
    postSlugFromIdentifier,
    postUrl,
    routeOrFail,
    sectionIndexUrl,
    slideSlugFromIdentifier,
    slideUrl,
  )

-- ============================================================================
-- Public API
-- ============================================================================

-- | Sitemap rules: generates sitemap.xml covering all public URLs.
sitemapRules :: SiteConfig -> Rules ()
sitemapRules cfg = do
  postDep <- makePatternDependency "content/posts/*/index.*.md"
  slideDep <- makePatternDependency "content/slides/*/slides.md"
  pageDep <- makePatternDependency standalonePagePattern
  rulesExtraDependencies [postDep, slideDep, pageDep] $
    create ["sitemap.xml"] $ do
      route idRoute
      compile $ do
        postFiles <- getMatches "content/posts/*/index.*.md"
        pageFiles <- getMatches standalonePagePattern
        slideFiles <- getMatches "content/slides/*/slides.md"
        let ls = map langCode $ languages cfg
            postSlugs = nubOrd $ rights $ map postSlugFromIdentifier postFiles
            pageSegments = nubOrd $ rights $ map pageSegmentsFromIdentifier pageFiles
        let root = T.unpack $ baseUrl $ site cfg
        slideEntryLists <- mapM (slideEntries root ls) slideFiles
        let entries =
              concatMap (staticPageEntries root) ls
                ++ concatMap (postEntries root ls) postSlugs
                ++ concatMap (pageEntries root ls) pageSegments
                ++ concat slideEntryLists
        makeItem ("" :: String)
          >>= loadAndApplyTemplate
            "templates/sitemap.xml"
            (listField "entries" entryCtx (mapM makeItem entries))

-- ============================================================================
-- Internal types and helpers
-- ============================================================================

data SitemapEntry = SitemapEntry {sitemapLoc :: String, sitemapPriority :: String}

standalonePagePattern :: Pattern
standalonePagePattern =
  "content/**/index.*.md"
    .&&. complement "content/index.*.md"
    .&&. complement "content/posts/**"
    .&&. complement "content/slides/**"

-- | Static page entries for a single language.
staticPageEntries :: String -> LangCode -> [SitemapEntry]
staticPageEntries root lang =
  [ SitemapEntry (root <> homeUrl lang) "1.0",
    SitemapEntry (root <> sectionIndexUrl lang Posts) "0.8",
    SitemapEntry (root <> sectionIndexUrl lang Slides) "0.8"
  ]

-- | Sitemap entries for a post across all configured languages, including fallback URLs.
postEntries :: String -> [LangCode] -> Slug -> [SitemapEntry]
postEntries root langs slug = [SitemapEntry (root <> postUrl lang slug) "0.6" | lang <- langs]

-- | Sitemap entries for a standalone page across all configured languages, including fallback URLs.
pageEntries :: String -> [LangCode] -> [T.Text] -> [SitemapEntry]
pageEntries root langs segments = [SitemapEntry (root <> pageUrl lang segments) "0.7" | lang <- langs]

-- | Sitemap entries for a slide across all configured languages.
slideEntries :: String -> [LangCode] -> Identifier -> Compiler [SitemapEntry]
slideEntries root langs identifier = do
  slug <- routeOrFail $ slideSlugFromIdentifier identifier
  pure [SitemapEntry (root <> slideUrl l slug) "0.6" | l <- langs]

entryCtx :: Context SitemapEntry
entryCtx =
  field "loc" (pure . sitemapLoc . itemBody)
    <> field "priority" (pure . sitemapPriority . itemBody)
