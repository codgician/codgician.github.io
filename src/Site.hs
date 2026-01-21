{-# LANGUAGE OverloadedStrings #-}

module Site
  ( hakyllMain,
  )
where

import Compiler.Pandoc (customPandocCompiler, slideCompiler)
import Config (FeedConfig (..), Language (..), NavItem (..), SiteConfig (..), SiteInfo (..), getTrans, langCodes, loadConfig, postsPerPage, slidesPerPage)
import Context
import Control.Monad (filterM, forM_)
import Data.Char (toLower)
import Data.List (groupBy, nub, sortBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (Down (..), comparing)
import qualified Data.Text as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import Feed (feedConfiguration, feedCtxForLang)
import Hakyll
import Hakyll.Web.Paginate (buildPaginateWith, paginateEvery, paginateRules)
import Paginate (makePageId, paginationCtx)
import System.FilePath (joinPath, splitDirectories, takeFileName, (</>))

-- ============================================================================
-- Constants
-- ============================================================================

-- | Supported image extensions for assets
imageExtensions :: [String]
imageExtensions = ["jpg", "png", "gif", "svg", "webp"]

-- ============================================================================
-- Configuration
-- ============================================================================

config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "_site",
      storeDirectory = "_cache",
      tmpDirectory = "_cache/tmp",
      providerDirectory = "."
    }

-- ============================================================================
-- Main Entry Point
-- ============================================================================

hakyllMain :: IO ()
hakyllMain = hakyllWith config rules

rules :: Rules ()
rules = do
  -- Load config inside Rules monad to enable dependency tracking
  cfg <- preprocess $ loadConfig "config.yaml"

  -- Create dependency on config.yaml - any change triggers rebuild
  configDep <- makePatternDependency "config.yaml"

  -- Static files don't need config dependency
  staticFiles
  scssCompilation
  templates
  errorPage

  -- All rules that use config values need the config dependency
  rulesExtraDependencies [configDep] $ do
    postAssets cfg
    homepages cfg
    standalonePages cfg
    blogPostSources cfg
    blogPostPages cfg
    postListPages cfg
    rssFeeds cfg
    sitemap cfg

    -- Slide rules
    slideAssets cfg
    slides cfg
    slideIndex cfg

-- ============================================================================
-- Static Assets
-- ============================================================================

staticFiles :: Rules ()
staticFiles =
  match ("static/**" .&&. complement "static/scss/**") $ do
    route $ gsubRoute "static/" (const "")
    compile copyFileCompiler

postAssets :: SiteConfig -> Rules ()
postAssets cfg =
  forM_ imageExtensions $ \ext ->
    match (fromGlob $ "content/posts/*/*." <> ext) $ do
      forM_ (langCodes cfg) $ \lang ->
        version (T.unpack lang) $ do
          route $ customRoute $ \ident ->
            let parts = splitDirectories $ toFilePath ident
                slug = parts !! 2
                filename = takeFileName $ toFilePath ident
             in T.unpack lang </> "posts" </> slug </> filename
          compile copyFileCompiler

scssCompilation :: Rules ()
scssCompilation = do
  match "static/scss/_*.scss" $ compile getResourceBody
  match "static/scss/style.scss" $ do
    route $ constRoute "css/style.css"
    compile $ do
      _ <- loadAll "static/scss/_*.scss" :: Compiler [Item String]
      path <- toFilePath <$> getUnderlying
      makeItem ""
        >>= withItemBody (const $ unixFilter "sass" ["--load-path=static/scss", "--style=compressed", path] "")

templates :: Rules ()
templates = do
  match "templates/*" $ compile templateBodyCompiler
  match "templates/partials/*" $ compile templateBodyCompiler

errorPage :: Rules ()
errorPage =
  match "content/404.html" $ do
    route $ constRoute "404.html"
    compile copyFileCompiler

-- ============================================================================
-- Page Compilation
-- ============================================================================

homepages :: SiteConfig -> Rules ()
homepages cfg =
  match "content/index.*.md" $ do
    route $ customRoute $ \ident -> extractLang ident <> "/index.html"
    compile $ do
      lang <- currentLang
      let ctx = allLangsCtx cfg lang "/" <> homeCtx cfg lang
      getResourceBody
        >>= applyAsTemplate ctx
        >>= loadAndApplyTemplate "templates/home.html" ctx

standalonePages :: SiteConfig -> Rules ()
standalonePages cfg =
  match ("content/**/index.*.md" .&&. complement "content/index.*.md" .&&. complement "content/posts/**") $ do
    route $ customRoute $ \ident ->
      let path = toFilePath ident
          lang = extractLang ident
          pathParts = splitDirectories path
          slugParts = drop 1 $ init pathParts
          slug = joinPath slugParts
       in lang </> slug </> "index.html"
    compile $ do
      ident <- getUnderlying
      lang <- currentLang
      langsCtx' <- nestedPageLangsCtx cfg lang ident
      (enableMath, enableMermaid) <- getFeatureFlags
      tpl <- getPageTemplate
      let ctx = langsCtx' <> pageCtx cfg lang tpl
      customPandocCompiler enableMath enableMermaid
        >>= loadAndApplyTemplate (fromFilePath $ "templates/" <> tpl <> ".html") ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

blogPostSources :: SiteConfig -> Rules ()
blogPostSources cfg =
  match "content/posts/*/index.*.md" $ do
    route $ customRoute $ \ident ->
      let parts = splitDirectories $ toFilePath ident
          slug = parts !! 2
          lang = extractLang ident
       in lang </> "posts" </> slug </> "index.html"
    compile $ do
      ident <- getUnderlying
      lang <- currentLang
      let slug = splitDirectories (toFilePath ident) !! 2
      (enableMath, enableMermaid) <- getFeatureFlags
      let ctx = allLangsCtx cfg lang ("/posts/" <> slug <> "/") <> postCtx cfg lang
      customPandocCompiler enableMath enableMermaid
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

blogPostPages :: SiteConfig -> Rules ()
blogPostPages cfg = do
  allPostIdents <- makePatternDependency "content/posts/*/index.*.md"
  rulesExtraDependencies [allPostIdents] $ do
    postFiles <- getMatches "content/posts/*/index.*.md"
    let slugs = nub $ map postSlug postFiles
    forM_ (languages cfg) $ \lang -> do
      let langStr = T.unpack $ langCode lang
      forM_ slugs $ \slug -> do
        let hasSource = any (\i -> extractLang i == langStr && postSlug i == slug) postFiles
        if hasSource
          then pure ()
          else createFallbackPost cfg langStr slug

createFallbackPost :: SiteConfig -> String -> String -> Rules ()
createFallbackPost cfg targetLang slug =
  create [fromFilePath $ targetLang </> "posts" </> slug </> "index.html"] $ do
    route idRoute
    compile $ do
      sourceIdent <- findFallbackSource cfg targetLang slug
      case sourceIdent of
        Nothing -> fail $ "No source found for post: " <> slug
        Just srcIdent -> do
          srcItem <- loadSnapshot srcIdent "content"
          srcMeta <- getMetadata srcIdent
          let ctx =
                constField "date" (metaString "date" srcMeta)
                  <> constField "title" (metaString "title" srcMeta)
                  <> boolCtx "math" srcMeta
                  <> boolCtx "mermaid" srcMeta
                  <> allLangsCtx cfg targetLang ("/posts/" <> slug <> "/")
                  <> postCtx cfg targetLang
          makeItem (itemBody srcItem)
            >>= loadAndApplyTemplate "templates/post.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

postListPages :: SiteConfig -> Rules ()
postListPages cfg = do
  allPostIdents <- makePatternDependency "content/posts/*/index.*.md"
  rulesExtraDependencies [allPostIdents] $
    forM_ (languages cfg) $ \lang -> do
      let langStr = T.unpack $ langCode lang
          pageTitle = navTitle cfg langStr "posts/"
          perPage = postsPerPage cfg

      -- Build paginator for this language
      paginate <- buildPaginateWith
        (\ids -> do
          sorted <- sortRecentFirst ids
          return $ paginateEvery perPage sorted)
        (fromGlob $ "content/posts/*/index." <> langStr <> ".md")
        (makePageId langStr "posts")

      -- Generate rules for each page
      paginateRules paginate $ \pageNum pattern -> do
        route idRoute
        compile $ do
          posts <- recentFirst =<< loadAllSnapshots pattern "content"
          yearGroups <- groupByYear posts

          let listItemCtx = postListItemCtx langStr
              ctx =
                constField "title" pageTitle
                  <> listField "yearGroups" (yearGroupsCtx listItemCtx) (pure $ map toItem yearGroups)
                  <> paginationCtx paginate pageNum
                  <> allLangsCtx cfg langStr "/posts/"
                  <> baseCtx cfg langStr

          makeItem ""
            >>= loadAndApplyTemplate "templates/post-list.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

rssFeeds :: SiteConfig -> Rules ()
rssFeeds cfg = do
  let feedCount = feedItemsCount $ feed cfg
  -- Add dependency on all posts so feed rebuilds when any post changes
  postDep <- makePatternDependency "content/posts/*/index.*.md"
  rulesExtraDependencies [postDep] $
    forM_ (map (T.unpack . langCode) $ languages cfg) $ \lang ->
      create [fromFilePath $ lang </> "feed.xml"] $ do
        route idRoute
        compile $ do
          -- Get all post slugs
          allPostFiles <- getMatches "content/posts/*/index.*.md"
          let slugs = nub $ map postSlug allPostFiles

          -- Load best available content for each slug (native or fallback)
          posts <- fmap catMaybes $ mapM (bestPostForLang cfg lang) slugs
          sortedPosts <- fmap (take feedCount) $ recentFirst posts
          renderAtom (feedConfiguration cfg lang) (feedCtxForLang lang) sortedPosts

sitemap :: SiteConfig -> Rules ()
sitemap cfg = do
  -- Create dependencies on all content that should appear in sitemap
  postDep <- makePatternDependency "content/posts/*/index.*.md"
  slideDep <- makePatternDependency "content/slides/*/slides.md"
  rulesExtraDependencies [postDep, slideDep] $
    create ["sitemap.xml"] $ do
      route idRoute
      compile $ do
        -- Load all posts (native language only, not fallbacks)
        allPosts <- loadAllSnapshots "content/posts/*/index.*.md" "content" :: Compiler [Item String]

        -- Load slides (just one version to get the slugs, we'll generate entries for all langs)
        let defaultLang = T.unpack $ langCode $ head $ languages cfg
        allSlides <- loadAllSnapshots ("content/slides/*/slides.md" .&&. hasVersion defaultLang) "content" :: Compiler [Item String]

        let rootUrl = T.unpack $ baseUrl $ site cfg
            langs = map (T.unpack . langCode) $ languages cfg

            -- Static pages (homepage, list pages, about) for each language
            staticPages = concatMap (staticPagesForLang rootUrl) langs

            -- Post entries with clean URLs
            postEntries = map (toPostEntry rootUrl) allPosts

            -- Slide entries with clean URLs (one per slide per language)
            slideEntries = concatMap (toSlideEntries rootUrl langs) allSlides

            allEntries = staticPages ++ postEntries ++ slideEntries
            sitemapCtx = listField "entries" sitemapEntryCtx (mapM makeItem allEntries)

        makeItem "" >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
  where
    -- Static pages for a language
    staticPagesForLang root lang =
      [ SitemapEntry (root <> "/" <> lang <> "/") "1.0"           -- homepage
      , SitemapEntry (root <> "/" <> lang <> "/posts/") "0.8"     -- blog list
      , SitemapEntry (root <> "/" <> lang <> "/slides/") "0.8"    -- slides list
      , SitemapEntry (root <> "/" <> lang <> "/about/") "0.7"     -- about
      ]

    -- Create sitemap entry from post item
    toPostEntry root item =
      let path = toFilePath $ itemIdentifier item
          parts = splitDirectories path
          -- content/posts/{slug}/index.{lang}.md -> parts = ["content", "posts", slug, "index.{lang}.md"]
          slug = parts !! 2
          filename = parts !! 3
          lang = takeWhile (/= '.') $ drop 6 filename  -- "index.{lang}.md" -> lang
          url = root <> "/" <> lang <> "/posts/" <> slug <> "/"
       in SitemapEntry url "0.6"

    -- Slide entries for all language versions
    toSlideEntries root langs item =
      let path = toFilePath $ itemIdentifier item
          parts = splitDirectories path
          slug = parts !! 2
       in [ SitemapEntry (root <> "/" <> lang <> "/slides/" <> slug <> "/") "0.6"
          | lang <- langs
          ]

-- | Sitemap entry data
data SitemapEntry = SitemapEntry
  { sitemapLoc :: String
  , sitemapPriority :: String
  }

-- | Context for sitemap entries
sitemapEntryCtx :: Context SitemapEntry
sitemapEntryCtx =
  field "loc" (pure . sitemapLoc . itemBody)
    <> field "priority" (pure . sitemapPriority . itemBody)

-- ============================================================================
-- Slide Compilation
-- ============================================================================

-- | Copy slide assets (images in slide directories) - per language
slideAssets :: SiteConfig -> Rules ()
slideAssets cfg =
  forM_ imageExtensions $ \ext ->
    match (fromGlob $ "content/slides/*/images/*." <> ext) $ do
      forM_ (languages cfg) $ \lang ->
        version (T.unpack $ langCode lang) $ do
          route $ customRoute $ \ident ->
            let parts = splitDirectories $ toFilePath ident
                slug = parts !! 2
                filename = takeFileName $ toFilePath ident
             in T.unpack (langCode lang) </> "slides" </> slug </> "images" </> filename
          compile copyFileCompiler

-- | Compile slides using Pandoc's reveal.js writer - per language
slides :: SiteConfig -> Rules ()
slides cfg =
  match "content/slides/*/slides.md" $ do
    forM_ (languages cfg) $ \lang ->
      version (T.unpack $ langCode lang) $ do
        route $ customRoute $ \ident ->
          let parts = splitDirectories $ toFilePath ident
              slug = parts !! 2
           in T.unpack (langCode lang) </> "slides" </> slug </> "index.html"
        compile $ do
          metadata <- getUnderlying >>= getMetadata
          let enableMath = metaBool "math" metadata
          slideCompiler enableMath
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/slide.html" defaultContext
            >>= relativizeUrls

-- | Create slide index page - per language (paginated)
slideIndex :: SiteConfig -> Rules ()
slideIndex cfg = do
  slideDep <- makePatternDependency "content/slides/*/slides.md"
  rulesExtraDependencies [slideDep] $
    forM_ (languages cfg) $ \lang -> do
      let langStr = T.unpack $ langCode lang
          perPage = slidesPerPage cfg

      -- Build paginator for slides
      -- Note: We use the base pattern without hasVersion because getMatches
      -- works on source files, not compiled versions. The version is used
      -- when loading snapshots.
      paginate <- buildPaginateWith
        (\ids -> do
          sorted <- sortRecentFirst ids
          return $ paginateEvery perPage sorted)
        (fromGlob "content/slides/*/slides.md")
        (makePageId langStr "slides")

      paginateRules paginate $ \pageNum _pattern -> do
        route idRoute
        compile $ do
          -- Load slides with the specific language version
          -- We load from the versioned items directly since that's where snapshots are saved
          allSlides <- loadAllSnapshots (fromGlob "content/slides/*/slides.md" .&&. hasVersion langStr) "content"

          let pageTitle = navTitle cfg langStr "slides/"
              slideItemCtx =
                field "url" (makeSlideUrl langStr)
                  <> field "date" getSlideDate
                  <> defaultContext
              ctx =
                constField "title" pageTitle
                  <> listField "slides" slideItemCtx (pure allSlides)
                  <> paginationCtx paginate pageNum
                  <> allLangsCtx cfg langStr "/slides/"
                  <> baseCtx cfg langStr

          makeItem ""
            >>= loadAndApplyTemplate "templates/slide-list.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls
  where
    makeSlideUrl langStr item =
      let parts = splitDirectories $ toFilePath $ itemIdentifier item
          slug = parts !! 2
       in pure $ "/" <> langStr <> "/slides/" <> slug <> "/"

    -- Get date from metadata, return empty string if missing/unparseable
    getSlideDate item = do
      metadata <- getMetadata (itemIdentifier item)
      pure $ fromMaybe "" $ lookupString "date" metadata

-- ============================================================================
-- Context Builders (page-specific compositions)
-- ============================================================================

-- | Context for blog posts: base + post metadata
postCtx :: SiteConfig -> String -> Context String
postCtx cfg lang = baseCtx cfg lang <> postMetaCtx

-- | Context for standalone pages: base + optional extras based on template
pageCtx :: SiteConfig -> String -> String -> Context String
pageCtx cfg lang tpl = case tpl of
  "about" -> baseCtx cfg lang <> friendsCtx cfg
  _       -> baseCtx cfg lang

-- | Context for post list items (simplified, with custom URL)
postListItemCtx :: String -> Context String
postListItemCtx lang =
  field "url" makeUrl
    <> dateCtx
    <> defaultContext
  where
    makeUrl item =
      let slug = postSlug $ itemIdentifier item
       in pure $ "/" <> lang <> "/posts/" <> slug <> "/"

-- ============================================================================
-- Language Context Builders
-- ============================================================================

-- | All languages available (for pages that exist in all languages)
allLangsCtx :: SiteConfig -> String -> String -> Context String
allLangsCtx cfg currentLang urlSuffix =
  availableLangsCtx $ map toLang $ languages cfg
  where
    toLang lang =
      AvailableLang
        { alCode = T.unpack $ langCode lang,
          alLabel = T.unpack $ langLabel lang,
          alUrl = "/" <> T.unpack (langCode lang) <> urlSuffix,
          alActive = T.unpack (langCode lang) == currentLang
        }

-- | Languages available for nested pages (checks which translations exist)
nestedPageLangsCtx :: SiteConfig -> String -> Identifier -> Compiler (Context String)
nestedPageLangsCtx cfg currentLang ident = do
  let path = toFilePath ident
      pathParts = splitDirectories path
      slugParts = drop 1 $ init pathParts
      slug = joinPath slugParts
      contentDir = "content" </> joinPath (init slugParts)
  availLangs <- filterM (langFileExists contentDir) $ languages cfg
  pure $ availableLangsCtx $ map (toLang $ "/" <> slug <> "/") availLangs
  where
    toLang suffix lang =
      AvailableLang
        { alCode = T.unpack $ langCode lang,
          alLabel = T.unpack $ langLabel lang,
          alUrl = "/" <> T.unpack (langCode lang) <> suffix,
          alActive = T.unpack (langCode lang) == currentLang
        }

    langFileExists dir lang = do
      let path = dir </> "index." <> T.unpack (langCode lang) <> ".md"
      matches <- getMatches $ fromGlob path
      pure $ not $ null matches

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Extract language code from identifier (e.g., "index.en.md" -> "en")
extractLang :: Identifier -> String
extractLang ident =
  let filename = takeFileName $ toFilePath ident
   in takeWhile (/= '.') $ drop 1 $ dropWhile (/= '.') filename

-- | Get current language in Compiler monad
currentLang :: Compiler String
currentLang = extractLang <$> getUnderlying

-- | Extract post slug from identifier
postSlug :: Identifier -> String
postSlug ident = splitDirectories (toFilePath ident) !! 2

-- | Get feature flags from frontmatter
getFeatureFlags :: Compiler (Bool, Bool)
getFeatureFlags = do
  metadata <- getUnderlying >>= getMetadata
  let enableMath = metaBool "math" metadata
      enableMermaid = metaBool "mermaid" metadata
  pure (enableMath, enableMermaid)

-- | Get page template from frontmatter (default: "page")
getPageTemplate :: Compiler FilePath
getPageTemplate = do
  metadata <- getUnderlying >>= getMetadata
  pure $ fromMaybe "page" $ lookupString "template" metadata

-- | Get navigation title for a URL
navTitle :: SiteConfig -> String -> T.Text -> String
navTitle cfg lang url =
  let matchingNav = filter ((== url) . navUrl) $ navigation cfg
   in case matchingNav of
        (n : _) -> T.unpack $ getTrans (languages cfg) lang $ navLabel n
        [] -> T.unpack url

-- | Find post identifier for a given slug with language fallback
-- Tries target language first, then falls back to other available languages
findPostIdent :: SiteConfig -> String -> String -> Compiler (Maybe Identifier)
findPostIdent cfg targetLang slug = go langOrder
  where
    langOrder = targetLang : filter (/= targetLang) (map (T.unpack . langCode) $ languages cfg)

    go [] = pure Nothing
    go (l : ls) = do
      let pat = fromGlob $ "content/posts/" <> slug <> "/index." <> l <> ".md"
      matches <- getMatches pat
      case matches of
        (ident : _) -> pure $ Just ident
        [] -> go ls

-- | Find fallback source for a post (alias for findPostIdent)
findFallbackSource :: SiteConfig -> String -> String -> Compiler (Maybe Identifier)
findFallbackSource = findPostIdent

-- | Find best post for a language (with fallback), loading the content
bestPostForLang :: SiteConfig -> String -> String -> Compiler (Maybe (Item String))
bestPostForLang cfg targetLang slug = do
  mIdent <- findPostIdent cfg targetLang slug
  traverse (`loadSnapshot` "content") mIdent

-- | Group posts by year
groupByYear :: [Item String] -> Compiler [YearGroup]
groupByYear posts = do
  postsWithYears <- mapM addYear posts
  let sorted = sortBy (comparing (Down . fst)) postsWithYears
      grouped = groupBy (\a b -> fst a == fst b) sorted
  pure $ map toYearGroup grouped
  where
    addYear item = do
      time <- getItemUTC defaultTimeLocale (itemIdentifier item)
      let year = formatTime defaultTimeLocale "%Y" time
      pure (year, item)

    toYearGroup items =
      YearGroup
        { ygYear = fst $ head items,
          ygPosts = map snd items
        }

-- | Metadata helpers
metaString :: String -> Metadata -> String
metaString key = fromMaybe "" . lookupString key

metaBool :: String -> Metadata -> Bool
metaBool key meta = case lookupString key meta of
  Just s  -> map toLower s == "true"
  Nothing -> False

-- | Create context field only when metadata boolean is true
--   Used for feature flags like math/mermaid in fallback posts
boolCtx :: String -> Metadata -> Context String
boolCtx key meta
  | metaBool key meta = constField key "true"
  | otherwise         = mempty
