{-# LANGUAGE OverloadedStrings #-}

module Site
  ( hakyllMain,
  )
where

import Compiler.Pandoc (customPandocCompiler)
import Config (FeedConfig (..), Language (..), NavItem (..), SiteConfig (..), SiteInfo (..), getTrans, loadConfig)
import Context (AvailableLang (..), YearGroup (..), availableLangsCtx, langCtx, postCtx, siteCtx, yearGroupCtx)
import Control.Monad (filterM, forM_)
import Data.List (groupBy, nub, sortBy)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ord (Down (..), comparing)
import qualified Data.Text as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import Feed (feedConfiguration, feedCtx)
import Hakyll
import System.FilePath (joinPath, splitDirectories, takeFileName, (</>))

config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "_site",
      storeDirectory = "_cache",
      tmpDirectory = "_cache/tmp",
      providerDirectory = "."
    }

hakyllMain :: IO ()
hakyllMain = do
  cfg <- loadConfig "config.yaml"
  hakyllWith config $ rules cfg

rules :: SiteConfig -> Rules ()
rules cfg = do
  staticFiles
  postAssets cfg
  scssCompilation
  templates
  homepages cfg
  standalonePages cfg
  blogPostSources cfg
  blogPostPages cfg
  postListPages cfg
  rssFeeds cfg
  sitemap cfg
  errorPage

getLangCodes :: SiteConfig -> [String]
getLangCodes = map (T.unpack . langCode) . languages

getDefaultLang :: SiteConfig -> String
getDefaultLang cfg = case languages cfg of
  (l : _) -> T.unpack $ langCode l
  [] -> "en"

staticFiles :: Rules ()
staticFiles =
  match ("static/**" .&&. complement "static/scss/**") $ do
    route $ gsubRoute "static/" (const "")
    compile copyFileCompiler

postAssets :: SiteConfig -> Rules ()
postAssets cfg =
  forM_ ["jpg", "png", "gif", "svg", "webp"] $ \ext ->
    match (fromGlob $ "content/posts/*/*." <> ext) $ do
      forM_ (getLangCodes cfg) $ \lang ->
        version lang $ do
          route $ customRoute $ \ident ->
            let parts = splitDirectories $ toFilePath ident
                slug = parts !! 2
                filename = takeFileName $ toFilePath ident
             in lang </> "posts" </> slug </> filename
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

homepages :: SiteConfig -> Rules ()
homepages cfg =
  match "content/index.*.md" $ do
    route $ customRoute $ \ident -> extractLang ident <> "/index.html"
    compile $ do
      lang <- getLang
      let ctx = langsCtxAll cfg lang "/" <> langCtx lang <> siteCtx cfg lang
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
      lang <- getLang
      langsCtx <- pageLangsCtxNested cfg lang ident
      (enableMath, enableMermaid) <- getFeatureFlags
      let ctx = langsCtx <> postCtx cfg lang
      customPandocCompiler enableMath enableMermaid
        >>= loadAndApplyTemplate "templates/page.html" ctx
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
      lang <- getLang
      let slug = splitDirectories (toFilePath ident) !! 2
      (enableMath, enableMermaid) <- getFeatureFlags
      let ctx = langsCtxAll cfg lang ("/posts/" <> slug <> "/") <> postCtx cfg lang
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
    let slugs = nub $ map getPostSlug postFiles
    forM_ (languages cfg) $ \lang -> do
      let langStr = T.unpack $ langCode lang
      forM_ slugs $ \slug -> do
        let hasSource = any (\i -> extractLang i == langStr && getPostSlug i == slug) postFiles
        if hasSource
          then pure ()
          else createFallbackPost cfg langStr slug
  where
    getPostSlug ident = splitDirectories (toFilePath ident) !! 2

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
                constField "date" (getDateFromMeta srcMeta)
                  <> constField "title" (getTitleFromMeta srcMeta)
                  <> langsCtxAll cfg targetLang ("/posts/" <> slug <> "/")
                  <> postCtx cfg targetLang
          makeItem (itemBody srcItem)
            >>= loadAndApplyTemplate "templates/post.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

getDateFromMeta :: Metadata -> String
getDateFromMeta meta = maybe "" id $ lookupString "date" meta

getTitleFromMeta :: Metadata -> String
getTitleFromMeta meta = maybe "" id $ lookupString "title" meta

findFallbackSource :: SiteConfig -> String -> String -> Compiler (Maybe Identifier)
findFallbackSource cfg targetLang slug = do
  let langOrder = targetLang : filter (/= targetLang) (getLangCodes cfg)
  findFirst langOrder
  where
    findFirst [] = pure Nothing
    findFirst (l : ls) = do
      let pattern = fromGlob $ "content/posts/" <> slug <> "/index." <> l <> ".md"
      matches <- getMatches pattern
      case matches of
        (ident : _) -> pure $ Just ident
        [] -> findFirst ls

postListPages :: SiteConfig -> Rules ()
postListPages cfg =
  forM_ (languages cfg) $ \lang -> do
    let langStr = T.unpack $ langCode lang
        title = getNavTitle cfg langStr "posts/"
    create [fromFilePath $ langStr </> "posts/index.html"] $ do
      route idRoute
      compile $ do
        allPostFiles <- getMatches "content/posts/*/index.*.md"
        let slugs = nub $ map getPostSlug allPostFiles
        posts <- catMaybes <$> mapM (getBestPost cfg langStr) slugs
        sortedPosts <- recentFirst posts
        yearGroups <- groupPostsByYear sortedPosts
        let postCtxWithLangUrl = postCtxForList cfg langStr
            ctx =
              constField "title" title
                <> listField "yearGroups" (yearGroupCtx postCtxWithLangUrl) (pure $ map mkYearGroupItem yearGroups)
                <> langsCtxAll cfg langStr "/posts/"
                <> langCtx langStr
                <> siteCtx cfg langStr
        makeItem ""
          >>= loadAndApplyTemplate "templates/post-list.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls
  where
    mkYearGroupItem yg = Item (fromFilePath "") yg

    getPostSlug :: Identifier -> String
    getPostSlug ident = splitDirectories (toFilePath ident) !! 2

    getBestPost :: SiteConfig -> String -> String -> Compiler (Maybe (Item String))
    getBestPost cfg' targetLang slug = do
      let langOrder = targetLang : filter (/= targetLang) (getLangCodes cfg')
      findFirstPost langOrder slug

    findFirstPost :: [String] -> String -> Compiler (Maybe (Item String))
    findFirstPost [] _ = pure Nothing
    findFirstPost (l : ls) slug = do
      let pattern = fromGlob $ "content/posts/" <> slug <> "/index." <> l <> ".md"
      matches <- getMatches pattern
      case matches of
        (ident : _) -> Just <$> loadSnapshot ident "content"
        [] -> findFirstPost ls slug

postCtxForList :: SiteConfig -> String -> Context String
postCtxForList cfg lang =
  field "url" makeUrl
    <> dateField "date" "%B %e, %Y"
    <> dateField "dateShort" "%b %d"
    <> dateField "dateYear" "%Y"
    <> langCtx lang
    <> siteCtx cfg lang
  where
    makeUrl item = do
      let ident = itemIdentifier item
          slug = splitDirectories (toFilePath ident) !! 2
      pure $ "/" <> lang <> "/posts/" <> slug <> "/"

getNavTitle :: SiteConfig -> String -> T.Text -> String
getNavTitle cfg lang url =
  let langs = languages cfg
      navItems = navigation cfg
      matchingNav = filter ((== url) . navUrl) navItems
   in case matchingNav of
        (n : _) -> T.unpack $ getTrans langs lang $ navLabel n
        [] -> T.unpack url

groupPostsByYear :: [Item String] -> Compiler [YearGroup]
groupPostsByYear posts = do
  postsWithYears <- mapM addYear posts
  let sorted = sortBy (comparing (Down . fst)) postsWithYears
      grouped = groupBy (\a b -> fst a == fst b) sorted
  pure $ map toYearGroup grouped
  where
    addYear :: Item String -> Compiler (String, Item String)
    addYear item = do
      time <- getItemUTC defaultTimeLocale (itemIdentifier item)
      let year = formatTime defaultTimeLocale "%Y" time
      pure (year, item)

    toYearGroup :: [(String, Item String)] -> YearGroup
    toYearGroup items =
      YearGroup
        { ygYear = fst $ head items,
          ygPosts = map snd items
        }

rssFeeds :: SiteConfig -> Rules ()
rssFeeds cfg = do
  let feedCount = feedItemsCount $ feed cfg
  forM_ (getLangCodes cfg) $ \lang ->
    create [fromFilePath $ lang </> "feed.xml"] $ do
      route idRoute
      compile $ do
        posts <-
          fmap (take feedCount) . recentFirst
            =<< loadAllSnapshots (fromGlob $ "content/posts/*/index." <> lang <> ".md") "content"
        renderAtom (feedConfiguration cfg lang) feedCtx posts

sitemap :: SiteConfig -> Rules ()
sitemap cfg =
  create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
      allPosts <- loadAllSnapshots "content/posts/*/index.*.md" "content"
      let rootUrl = T.unpack $ baseUrl $ site cfg
          pageCtx = constField "root" rootUrl <> dateField "lastmod" "%Y-%m-%d" <> defaultContext
          sitemapCtx = listField "pages" pageCtx (pure allPosts)
      makeItem "" >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

errorPage :: Rules ()
errorPage =
  match "content/404.html" $ do
    route $ constRoute "404.html"
    compile copyFileCompiler

extractLang :: Identifier -> String
extractLang ident =
  let filename = takeFileName $ toFilePath ident
   in takeWhile (/= '.') $ drop 1 $ dropWhile (/= '.') filename

getLang :: Compiler String
getLang = extractLang <$> getUnderlying

getFeatureFlags :: Compiler (Bool, Bool)
getFeatureFlags = do
  metadata <- getUnderlying >>= getMetadata
  let enableMath = lookupBool "math" metadata
      enableMermaid = lookupBool "mermaid" metadata
  pure (enableMath, enableMermaid)
  where
    lookupBool key meta = maybe False (== "true") $ lookupString key meta

langsCtxAll :: SiteConfig -> String -> String -> Context String
langsCtxAll cfg currentLang urlSuffix =
  availableLangsCtx $ map toLang $ languages cfg
  where
    toLang lang =
      AvailableLang
        { alCode = T.unpack $ langCode lang,
          alLabel = T.unpack $ langLabel lang,
          alUrl = "/" <> T.unpack (langCode lang) <> urlSuffix,
          alActive = T.unpack (langCode lang) == currentLang
        }

pageLangsCtx :: SiteConfig -> String -> Identifier -> Compiler (Context String)
pageLangsCtx cfg currentLang ident = do
  let slug = splitDirectories (toFilePath ident) !! 1
  availLangs <- filterM (hasFile $ "content" </> slug) $ languages cfg
  pure $ availableLangsCtx $ map (toLang $ "/" <> slug <> "/") availLangs
  where
    toLang suffix lang =
      AvailableLang
        { alCode = T.unpack $ langCode lang,
          alLabel = T.unpack $ langLabel lang,
          alUrl = "/" <> T.unpack (langCode lang) <> suffix,
          alActive = T.unpack (langCode lang) == currentLang
        }

pageLangsCtxNested :: SiteConfig -> String -> Identifier -> Compiler (Context String)
pageLangsCtxNested cfg currentLang ident = do
  let path = toFilePath ident
      pathParts = splitDirectories path
      slugParts = drop 1 $ init pathParts
      slug = joinPath slugParts
      contentDir = "content" </> joinPath (init slugParts)
  availLangs <- filterM (hasFile contentDir) $ languages cfg
  pure $ availableLangsCtx $ map (toLang $ "/" <> slug <> "/") availLangs
  where
    toLang suffix lang =
      AvailableLang
        { alCode = T.unpack $ langCode lang,
          alLabel = T.unpack $ langLabel lang,
          alUrl = "/" <> T.unpack (langCode lang) <> suffix,
          alActive = T.unpack (langCode lang) == currentLang
        }

hasFile :: FilePath -> Language -> Compiler Bool
hasFile dir lang = do
  let path = dir </> "index." <> T.unpack (langCode lang) <> ".md"
  matches <- getMatches $ fromGlob path
  pure $ not $ null matches
