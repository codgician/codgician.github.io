{-# LANGUAGE OverloadedStrings #-}

module Rules.Posts
  ( postRules,
    postListRules,
    loadAllPostsForLang,
  )
where

import Compiler.Pandoc (customPandocCompiler, customPandocCompilerWithToc)
import Compiler.RenderContext (defaultPandocRenderContext)
import Config (Language (..), SiteConfig (..), langCodes)
import Content.Fallback (preferredLangOrder)
import Content.Metadata (featuresFromMetadata, metadataBool)
import Content.Types (LangCode (..), RenderFeatures (..), Section (..), Slug (..), langCodeString, nubOrd, slugString)
import Context
  ( YearGroup (..),
    allLangsCtx,
    baseCtx,
    navTitle,
    postCtx,
    postListItemCtx,
    tagsFieldFromMeta,
    toItem,
    tocCtx,
    yearGroupsCtx,
  )
import Control.Monad (forM_, unless)
import Data.Either (rights)
import Data.List (groupBy, sortBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (Down (..), comparing)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Hakyll
import Paginate (makePageId)
import Routes
  ( langFromIndexIdentifier,
    postOutputPath,
    postSlugFromIdentifier,
    postUrl,
    routeErrorMessage,
    routeOrFail,
    sectionIndexUrl,
  )

-- ============================================================================
-- Public API
-- ============================================================================

-- | Compile source posts and create fallback posts for missing translations.
postRules :: SiteConfig -> Rules ()
postRules = blogPosts

-- | Create the post list page for each language.
postListRules :: SiteConfig -> Rules ()
postListRules = postList

-- | Load all posts for a language (with fallback to other languages).
-- Returns posts sorted by date descending.
loadAllPostsForLang :: SiteConfig -> LangCode -> Compiler [Item String]
loadAllPostsForLang cfg lang = do
  allFiles <- getMatches "content/posts/*/index.*.md"
  let slugs = nubOrd $ rights $ map postSlugFromIdentifier allFiles
  posts <- catMaybes <$> mapM (bestPostForLang cfg lang) slugs
  recentFirst posts

-- ============================================================================
-- Internal: post compilation
-- ============================================================================

blogPosts :: SiteConfig -> Rules ()
blogPosts cfg = do
  -- Compile source posts
  match "content/posts/*/index.*.md" $ do
    route $ customRoute $ \i ->
      either (error . routeErrorMessage) id $ do
        lang <- langFromIndexIdentifier i
        slug <- postSlugFromIdentifier i
        pure $ postOutputPath lang slug
    compile $ do
      ident <- getUnderlying
      lang <- routeOrFail $ langFromIndexIdentifier ident
      slug <- routeOrFail $ postSlugFromIdentifier ident
      meta <- getMetadata ident
      let features = featuresFromMetadata meta
          basePostCtx =
            allLangsCtx cfg lang (`postUrl` slug)
              <> postCtx cfg lang
      renderCtx <- unsafeCompiler $ defaultPandocRenderContext features
      if renderToc features
        then do
          (bodyItem, maybeToc) <- customPandocCompilerWithToc renderCtx
          tocItem <- makeItem (fromMaybe "" maybeToc)
          _ <- saveSnapshot "toc" tocItem
          let ctx = tocCtx lang maybeToc <> basePostCtx
          saveSnapshot "content" bodyItem
            >>= loadAndApplyTemplate "templates/post.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls
        else
          customPandocCompiler renderCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html" basePostCtx
            >>= loadAndApplyTemplate "templates/default.html" basePostCtx
            >>= relativizeUrls

  -- Create fallback posts for missing translations
  postDep <- makePatternDependency "content/posts/*/index.*.md"
  rulesExtraDependencies [postDep] $ do
    postFiles <- getMatches "content/posts/*/index.*.md"
    let slugs = nubOrd $ rights $ map postSlugFromIdentifier postFiles
    forM_ (langCodes cfg) $ \lang ->
      forM_ slugs $ \slug ->
        unless (hasPostTranslation lang slug postFiles) $
          createFallbackPost cfg lang slug

-- | Check whether a translation already exists for (lang, slug).
hasPostTranslation :: LangCode -> Slug -> [Identifier] -> Bool
hasPostTranslation lang slug =
  any $ \i ->
    langFromIndexIdentifier i == Right lang
      && postSlugFromIdentifier i == Right slug

createFallbackPost :: SiteConfig -> LangCode -> Slug -> Rules ()
createFallbackPost cfg requestedLang slug =
  create [fromFilePath $ postOutputPath requestedLang slug] $ do
    route idRoute
    compile $ do
      maybeSourceIdentifier <- findPostIdent cfg requestedLang slug
      case maybeSourceIdentifier of
        Nothing -> fail $ "No source found for post: " <> slugString slug
        Just sourceIdentifier -> renderFallbackPost cfg requestedLang slug sourceIdentifier

renderFallbackPost :: SiteConfig -> LangCode -> Slug -> Identifier -> Compiler (Item String)
renderFallbackPost cfg requestedLang slug sourceIdentifier = do
  srcItem <- loadSnapshot sourceIdentifier "content"
  srcMeta <- getMetadata sourceIdentifier
  let hasTocFlag = metadataBool "toc" srcMeta
  maybeToc <-
    if hasTocFlag
      then do
        tocStr <- itemBody <$> loadSnapshot sourceIdentifier "toc"
        pure $ if null tocStr then Nothing else Just tocStr
      else pure Nothing
  let ctx =
        tocCtx requestedLang maybeToc
          <> tagsFieldFromMeta requestedLang srcMeta
          <> constField "date" (formatFallbackDate (metaStr "date" srcMeta))
          <> constField "title" (metaStr "title" srcMeta)
          <> boolCtx "math" srcMeta
          <> boolCtx "mermaid" srcMeta
          <> boolCtx "tikz" srcMeta
          <> allLangsCtx cfg requestedLang (`postUrl` slug)
          <> postCtx cfg requestedLang
  makeItem (itemBody srcItem)
    >>= loadAndApplyTemplate "templates/post.html" ctx
    >>= loadAndApplyTemplate "templates/default.html" ctx
    >>= relativizeUrls

-- ============================================================================
-- Internal: post list
-- ============================================================================

postList :: SiteConfig -> Rules ()
postList cfg = do
  postDep <- makePatternDependency "content/posts/*/index.*.md"
  rulesExtraDependencies [postDep] $
    forM_ (languages cfg) $ \lang -> do
      let lc = langCode lang
      create [makePageId lc Posts 1] $ do
        route idRoute
        compile $ do
          posts <- loadAllPostsForLang cfg lc
          yearGroups <- groupByYear posts
          let ctx =
                constField "title" (navTitle cfg lc (T.pack "posts/"))
                  <> listField "yearGroups" (yearGroupsCtx $ postListItemCtx lc) (pure $ map toItem yearGroups)
                  <> allLangsCtx cfg lc (`sectionIndexUrl` Posts)
                  <> baseCtx cfg lc
          makeItem ""
            >>= loadAndApplyTemplate "templates/post-list.html" ctx
            >>= relativizeUrls

-- ============================================================================
-- Internal: post resolution helpers
-- ============================================================================

-- | Find the best source identifier for a post in the given language,
-- falling back to other configured languages in order.
findPostIdent :: SiteConfig -> LangCode -> Slug -> Compiler (Maybe Identifier)
findPostIdent cfg targetLang slug = go (preferredLangOrder cfg targetLang)
  where
    go [] = pure Nothing
    go (l : ls) = do
      postMatches <-
        getMatches $
          fromGlob $
            "content/posts/" <> slugString slug <> "/index." <> langCodeString l <> ".md"
      case postMatches of
        (i : _) -> pure $ Just i
        [] -> go ls

-- | Load the best available snapshot for a post in the given language.
bestPostForLang :: SiteConfig -> LangCode -> Slug -> Compiler (Maybe (Item String))
bestPostForLang cfg lang slug =
  traverse (`loadSnapshot` "content") =<< findPostIdent cfg lang slug

-- ============================================================================
-- Internal: year grouping
-- ============================================================================

groupByYear :: [Item String] -> Compiler [YearGroup]
groupByYear posts = do
  tagged <-
    mapM
      ( \p -> do
          time <- getItemUTC defaultTimeLocale (itemIdentifier p)
          pure (formatTime defaultTimeLocale "%Y" time, p)
      )
      posts
  pure
    [ YearGroup y (map snd ps)
    | ps@((y, _) : _) <- groupBy (\a b -> fst a == fst b) $ sortBy (comparing (Down . fst)) tagged
    ]

-- ============================================================================
-- Internal: metadata helpers
-- ============================================================================

metaStr :: String -> Metadata -> String
metaStr k = fromMaybe "" . lookupString k

-- | Format a raw YYYY-MM-DD date string as "Month D, YYYY".
-- If parsing fails, the raw string is returned unchanged.
formatFallbackDate :: String -> String
formatFallbackDate raw =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" raw of
    Just day -> formatTime defaultTimeLocale "%B %e, %Y" (day :: Day)
    Nothing -> raw

boolCtx :: String -> Metadata -> Context String
boolCtx k m = if metadataBool k m then constField k "true" else mempty
