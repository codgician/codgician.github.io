{-# LANGUAGE OverloadedStrings #-}

module Site (hakyllMain) where

import Compiler.Pandoc (customPandocCompiler, slideCompiler)
import Config
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
-- Constants & Configuration
-- ============================================================================

imageExtensions :: [String]
imageExtensions = ["jpg", "png", "gif", "svg", "webp"]

config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "_site"
  , storeDirectory = "_cache"
  , tmpDirectory = "_cache/tmp"
  , providerDirectory = "."
  }

-- ============================================================================
-- Main Entry Point
-- ============================================================================

hakyllMain :: IO ()
hakyllMain = hakyllWith config rules

rules :: Rules ()
rules = do
  cfg <- preprocess $ loadConfig "config.yaml"
  configDep <- makePatternDependency "config.yaml"

  -- Static files (no config dependency needed)
  staticFiles
  scssCompilation
  templates
  errorPage

  -- Content rules (depend on config)
  rulesExtraDependencies [configDep] $ do
    contentAssets cfg
    homepages cfg
    standalonePages cfg
    blogPosts cfg
    postList cfg
    rssFeeds cfg
    sitemap cfg
    slidePages cfg
    slideList cfg

-- ============================================================================
-- Static Assets
-- ============================================================================

staticFiles :: Rules ()
staticFiles =
  match ("static/**" .&&. complement "static/scss/**") $ do
    route $ gsubRoute "static/" (const "")
    compile copyFileCompiler

-- | Copy images from content directories to output (posts and slides)
contentAssets :: SiteConfig -> Rules ()
contentAssets cfg = forM_ imageExtensions $ \ext -> do
  -- Post images: content/posts/{slug}/*.ext -> {lang}/posts/{slug}/*.ext
  match (fromGlob $ "content/posts/*/*." <> ext) $
    forM_ (langCodes cfg) $ \lang ->
      version (T.unpack lang) $ do
        route $ customRoute $ \i -> T.unpack lang </> "posts" </> slugFrom i </> takeFileName (toFilePath i)
        compile copyFileCompiler
  -- Slide images: content/slides/{slug}/*.ext -> {lang}/slides/{slug}/*.ext
  match (fromGlob $ "content/slides/*/*." <> ext) $
    forM_ (langCodes cfg) $ \lang ->
      version (T.unpack lang) $ do
        route $ customRoute $ \i -> T.unpack lang </> "slides" </> slugFrom i </> takeFileName (toFilePath i)
        compile copyFileCompiler
  where
    slugFrom i = splitDirectories (toFilePath i) !! 2

scssCompilation :: Rules ()
scssCompilation = do
  match "static/scss/_*.scss" $ compile getResourceBody
  match "static/scss/style.scss" $ do
    route $ constRoute "css/style.css"
    compile $ do
      _ <- loadAll "static/scss/_*.scss" :: Compiler [Item String]
      path <- toFilePath <$> getUnderlying
      makeItem "" >>= withItemBody (const $ unixFilter "sass" ["--load-path=static/scss", "--style=compressed", path] "")

templates :: Rules ()
templates = do
  match "templates/*" $ compile templateBodyCompiler
  match "templates/partials/*" $ compile templateBodyCompiler

errorPage :: Rules ()
errorPage = match "content/404.html" $ do
  route $ constRoute "404.html"
  compile copyFileCompiler

-- ============================================================================
-- Pages
-- ============================================================================

homepages :: SiteConfig -> Rules ()
homepages cfg = match "content/index.*.md" $ do
  route $ customRoute $ \i -> extractLang i <> "/index.html"
  compile $ do
    lang <- currentLang
    let ctx = allLangsCtx cfg lang "/" <> homeCtx cfg lang
    getResourceBody >>= applyAsTemplate ctx >>= loadAndApplyTemplate "templates/home.html" ctx

standalonePages :: SiteConfig -> Rules ()
standalonePages cfg =
  match ("content/**/index.*.md" .&&. complement "content/index.*.md" .&&. complement "content/posts/**") $ do
    route $ customRoute $ \i ->
      let parts = splitDirectories $ toFilePath i
          slug = joinPath $ drop 1 $ init parts
       in extractLang i </> slug </> "index.html"
    compile $ do
      ident <- getUnderlying
      lang <- currentLang
      langsCtx' <- nestedPageLangsCtx cfg lang ident
      (enableMath, enableMermaid) <- getFeatureFlags
      tpl <- fromMaybe "page" . lookupString "template" <$> (getUnderlying >>= getMetadata)
      let ctx = langsCtx' <> pageCtx cfg lang tpl
      customPandocCompiler enableMath enableMermaid
        >>= loadAndApplyTemplate (fromFilePath $ "templates/" <> tpl <> ".html") ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

-- ============================================================================
-- Blog Posts
-- ============================================================================

blogPosts :: SiteConfig -> Rules ()
blogPosts cfg = do
  -- Compile source posts
  match "content/posts/*/index.*.md" $ do
    route $ customRoute $ \i ->
      let slug = splitDirectories (toFilePath i) !! 2
       in extractLang i </> "posts" </> slug </> "index.html"
    compile $ do
      lang <- currentLang
      slug <- (!! 2) . splitDirectories . toFilePath <$> getUnderlying
      (enableMath, enableMermaid) <- getFeatureFlags
      let ctx = allLangsCtx cfg lang ("/posts/" <> slug <> "/") <> postCtx cfg lang
      customPandocCompiler enableMath enableMermaid
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  -- Create fallback posts for missing translations
  postDep <- makePatternDependency "content/posts/*/index.*.md"
  rulesExtraDependencies [postDep] $ do
    postFiles <- getMatches "content/posts/*/index.*.md"
    let slugs = nub $ map postSlug postFiles
    forM_ (languages cfg) $ \lang -> do
      let langStr = T.unpack $ langCode lang
      forM_ slugs $ \slug ->
        unless (any (\i -> extractLang i == langStr && postSlug i == slug) postFiles) $
          createFallbackPost cfg langStr slug
  where
    unless cond action = if cond then pure () else action

createFallbackPost :: SiteConfig -> String -> String -> Rules ()
createFallbackPost cfg targetLang slug =
  create [fromFilePath $ targetLang </> "posts" </> slug </> "index.html"] $ do
    route idRoute
    compile $ do
      mSrcIdent <- findPostIdent cfg targetLang slug
      case mSrcIdent of
        Nothing -> fail $ "No source found for post: " <> slug
        Just srcIdent -> do
          srcItem <- loadSnapshot srcIdent "content"
          srcMeta <- getMetadata srcIdent
          let ctx = constField "date" (metaStr "date" srcMeta)
                 <> constField "title" (metaStr "title" srcMeta)
                 <> boolCtx "math" srcMeta <> boolCtx "mermaid" srcMeta
                 <> allLangsCtx cfg targetLang ("/posts/" <> slug <> "/")
                 <> postCtx cfg targetLang
          makeItem (itemBody srcItem)
            >>= loadAndApplyTemplate "templates/post.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

-- ============================================================================
-- Post List & RSS Feeds
-- ============================================================================

-- | Load all posts for a language (with fallback)
loadAllPostsForLang :: SiteConfig -> String -> Compiler [Item String]
loadAllPostsForLang cfg lang = do
  allFiles <- getMatches "content/posts/*/index.*.md"
  let slugs = nub $ map postSlug allFiles
  posts <- catMaybes <$> mapM (bestPostForLang cfg lang) slugs
  recentFirst posts

postList :: SiteConfig -> Rules ()
postList cfg = do
  postDep <- makePatternDependency "content/posts/*/index.*.md"
  rulesExtraDependencies [postDep] $
    forM_ (languages cfg) $ \lang -> do
      let langStr = T.unpack $ langCode lang
          perPage = postsPerPage cfg

      allFiles <- getMatches "content/posts/*/index.*.md"
      let numPosts = length $ nub $ map postSlug allFiles
          numPages = max 1 $ (numPosts + perPage - 1) `div` perPage

      forM_ [1 .. numPages] $ \pageNum ->
        create [fromFilePath $ pagePath langStr pageNum] $ do
          route idRoute
          compile $ do
            posts <- loadAllPostsForLang cfg langStr
            let pagePosts = take perPage $ drop ((pageNum - 1) * perPage) posts
            yearGroups <- groupByYear pagePosts
            let ctx = constField "title" (navTitle cfg langStr "posts/")
                   <> listField "yearGroups" (yearGroupsCtx $ postListItemCtx langStr) (pure $ map toItem yearGroups)
                   <> paginationCtx' langStr pageNum numPages
                   <> allLangsCtx cfg langStr "/posts/"
                   <> baseCtx cfg langStr
            makeItem ""
              >>= loadAndApplyTemplate "templates/post-list.html" ctx
              >>= loadAndApplyTemplate "templates/default.html" ctx
              >>= relativizeUrls
  where
    pagePath lang n = if n == 1 then lang </> "posts/index.html" else lang </> "posts/page" </> show n </> "index.html"
    paginationCtx' lang cur total =
      constField "hasPagination" "true"
        <> constField "currentPageNum" (show cur)
        <> constField "numPages" (show total)
        <> (if cur > 1 then constField "previousPageUrl" (prevUrl lang cur) else mempty)
        <> (if cur < total then constField "nextPageUrl" ("/" <> lang <> "/posts/page/" <> show (cur + 1) <> "/") else mempty)
    prevUrl lang cur = if cur == 2 then "/" <> lang <> "/posts/" else "/" <> lang <> "/posts/page/" <> show (cur - 1) <> "/"

rssFeeds :: SiteConfig -> Rules ()
rssFeeds cfg = do
  postDep <- makePatternDependency "content/posts/*/index.*.md"
  rulesExtraDependencies [postDep] $
    forM_ (languages cfg) $ \lang -> do
      let langStr = T.unpack $ langCode lang
      create [fromFilePath $ langStr </> "feed.xml"] $ do
        route idRoute
        compile $ do
          posts <- take (feedItemsCount $ feed cfg) <$> loadAllPostsForLang cfg langStr
          renderAtom (feedConfiguration cfg langStr) (feedCtxForLang langStr) posts

-- ============================================================================
-- Sitemap
-- ============================================================================

data SitemapEntry = SitemapEntry { sitemapLoc :: String, sitemapPriority :: String }

sitemap :: SiteConfig -> Rules ()
sitemap cfg = do
  postDep <- makePatternDependency "content/posts/*/index.*.md"
  slideDep <- makePatternDependency "content/slides/*/slides.md"
  rulesExtraDependencies [postDep, slideDep] $
    create ["sitemap.xml"] $ do
      route idRoute
      compile $ do
        posts <- loadAllSnapshots "content/posts/*/index.*.md" "content" :: Compiler [Item String]
        let defaultLang = T.unpack $ langCode $ head $ languages cfg
        slides <- loadAllSnapshots ("content/slides/*/slides.md" .&&. hasVersion defaultLang) "content" :: Compiler [Item String]
        let root = T.unpack $ baseUrl $ site cfg
            langs = map (T.unpack . langCode) $ languages cfg
            entries = concatMap (staticPages root) langs
                   ++ map (postEntry root) posts
                   ++ concatMap (slideEntries root langs) slides
        makeItem "" >>= loadAndApplyTemplate "templates/sitemap.xml" (listField "entries" entryCtx (mapM makeItem entries))
  where
    staticPages root lang = map (\(p, pr) -> SitemapEntry (root <> "/" <> lang <> p) pr)
      [("/", "1.0"), ("/posts/", "0.8"), ("/slides/", "0.8"), ("/about/", "0.7")]
    postEntry root item =
      let parts = splitDirectories $ toFilePath $ itemIdentifier item
          slug = parts !! 2
          lang = takeWhile (/= '.') $ drop 6 $ parts !! 3
       in SitemapEntry (root <> "/" <> lang <> "/posts/" <> slug <> "/") "0.6"
    slideEntries root langs item =
      let slug = splitDirectories (toFilePath $ itemIdentifier item) !! 2
       in [SitemapEntry (root <> "/" <> l <> "/slides/" <> slug <> "/") "0.6" | l <- langs]
    entryCtx = field "loc" (pure . sitemapLoc . itemBody) <> field "priority" (pure . sitemapPriority . itemBody)

-- ============================================================================
-- Slides
-- ============================================================================

slidePages :: SiteConfig -> Rules ()
slidePages cfg = match "content/slides/*/slides.md" $
  forM_ (languages cfg) $ \lang ->
    version (T.unpack $ langCode lang) $ do
      route $ customRoute $ \i ->
        let slug = splitDirectories (toFilePath i) !! 2
         in T.unpack (langCode lang) </> "slides" </> slug </> "index.html"
      compile $ do
        enableMath <- metaBool "math" <$> (getUnderlying >>= getMetadata)
        slideCompiler enableMath
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/slide.html" defaultContext
          >>= relativizeUrls

slideList :: SiteConfig -> Rules ()
slideList cfg = do
  slideDep <- makePatternDependency "content/slides/*/slides.md"
  rulesExtraDependencies [slideDep] $
    forM_ (languages cfg) $ \lang -> do
      let langStr = T.unpack $ langCode lang
      paginate <- buildPaginateWith
        (\ids -> paginateEvery (slidesPerPage cfg) <$> sortRecentFirst ids)
        (fromGlob "content/slides/*/slides.md")
        (makePageId langStr "slides")
      paginateRules paginate $ \pageNum _ -> do
        route idRoute
        compile $ do
          slides <- loadAllSnapshots (fromGlob "content/slides/*/slides.md" .&&. hasVersion langStr) "content"
          let slideCtx = field "url" (makeUrl langStr) <> field "date" getDate <> defaultContext
              ctx = constField "title" (navTitle cfg langStr "slides/")
                 <> listField "slides" slideCtx (pure slides)
                 <> paginationCtx paginate pageNum
                 <> allLangsCtx cfg langStr "/slides/"
                 <> baseCtx cfg langStr
          makeItem ""
            >>= loadAndApplyTemplate "templates/slide-list.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls
  where
    makeUrl lang item = pure $ "/" <> lang <> "/slides/" <> (splitDirectories (toFilePath $ itemIdentifier item) !! 2) <> "/"
    getDate item = fromMaybe "" . lookupString "date" <$> getMetadata (itemIdentifier item)

-- ============================================================================
-- Contexts
-- ============================================================================

postCtx :: SiteConfig -> String -> Context String
postCtx cfg lang = baseCtx cfg lang <> postMetaCtx

pageCtx :: SiteConfig -> String -> String -> Context String
pageCtx cfg lang "about" = baseCtx cfg lang <> friendsCtx cfg
pageCtx cfg lang _ = baseCtx cfg lang

postListItemCtx :: String -> Context String
postListItemCtx lang = field "url" makeUrl <> dateCtx <> defaultContext
  where makeUrl item = pure $ "/" <> lang <> "/posts/" <> postSlug (itemIdentifier item) <> "/"

allLangsCtx :: SiteConfig -> String -> String -> Context String
allLangsCtx cfg currentLang urlSuffix = availableLangsCtx
  [ AvailableLang (T.unpack $ langCode l) (T.unpack $ langLabel l) ("/" <> T.unpack (langCode l) <> urlSuffix) (T.unpack (langCode l) == currentLang)
  | l <- languages cfg ]

nestedPageLangsCtx :: SiteConfig -> String -> Identifier -> Compiler (Context String)
nestedPageLangsCtx cfg currentLang ident = do
  let parts = splitDirectories $ toFilePath ident
      slug = joinPath $ drop 1 $ init parts
      dir = "content" </> joinPath (init $ drop 1 parts)
  availLangs <- filterM (langExists dir) $ languages cfg
  pure $ availableLangsCtx
    [ AvailableLang (T.unpack $ langCode l) (T.unpack $ langLabel l) ("/" <> T.unpack (langCode l) <> "/" <> slug <> "/") (T.unpack (langCode l) == currentLang)
    | l <- availLangs ]
  where
    langExists dir l = not . null <$> getMatches (fromGlob $ dir </> "index." <> T.unpack (langCode l) <> ".md")

-- ============================================================================
-- Helpers
-- ============================================================================

extractLang :: Identifier -> String
extractLang i = takeWhile (/= '.') $ drop 1 $ dropWhile (/= '.') $ takeFileName $ toFilePath i

currentLang :: Compiler String
currentLang = extractLang <$> getUnderlying

postSlug :: Identifier -> String
postSlug i = splitDirectories (toFilePath i) !! 2

getFeatureFlags :: Compiler (Bool, Bool)
getFeatureFlags = do
  meta <- getUnderlying >>= getMetadata
  pure (metaBool "math" meta, metaBool "mermaid" meta)

navTitle :: SiteConfig -> String -> T.Text -> String
navTitle cfg lang url = case filter ((== url) . navUrl) $ navigation cfg of
  (n : _) -> T.unpack $ getTrans (languages cfg) lang $ navLabel n
  [] -> T.unpack url

findPostIdent :: SiteConfig -> String -> String -> Compiler (Maybe Identifier)
findPostIdent cfg targetLang slug = go $ targetLang : filter (/= targetLang) (map (T.unpack . langCode) $ languages cfg)
  where
    go [] = pure Nothing
    go (l : ls) = do
      matches <- getMatches $ fromGlob $ "content/posts/" <> slug <> "/index." <> l <> ".md"
      case matches of
        (i : _) -> pure $ Just i
        [] -> go ls

bestPostForLang :: SiteConfig -> String -> String -> Compiler (Maybe (Item String))
bestPostForLang cfg lang slug = traverse (`loadSnapshot` "content") =<< findPostIdent cfg lang slug

groupByYear :: [Item String] -> Compiler [YearGroup]
groupByYear posts = do
  tagged <- mapM (\p -> do
    time <- getItemUTC defaultTimeLocale (itemIdentifier p)
    pure (formatTime defaultTimeLocale "%Y" time, p)) posts
  pure [YearGroup y (map snd ps) | ps@((y, _) : _) <- groupBy (\a b -> fst a == fst b) $ sortBy (comparing (Down . fst)) tagged]

metaStr :: String -> Metadata -> String
metaStr k = fromMaybe "" . lookupString k

metaBool :: String -> Metadata -> Bool
metaBool k m = maybe False ((== "true") . map toLower) $ lookupString k m

boolCtx :: String -> Metadata -> Context String
boolCtx k m = if metaBool k m then constField k "true" else mempty
