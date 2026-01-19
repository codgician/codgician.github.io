{-# LANGUAGE OverloadedStrings #-}

module Site
  ( hakyllMain,
  )
where

import Compiler.Pandoc (customPandocCompiler)
import Config (FeedConfig (..), Language (..), SiteConfig (..), SiteInfo (..), loadConfig)
import Context (AvailableLang (..), availableLangsCtx, langCtx, postCtx, siteCtx)
import Control.Monad (filterM)
import qualified Data.Text as T
import Feed (feedConfiguration, feedCtx)
import Hakyll
import System.FilePath (replaceExtension, splitDirectories, takeFileName, (</>))

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
  hakyllWith config $ do
    -- Copy static files (excluding SCSS source files)
    match ("static/**" .&&. complement "static/scss/**") $ do
      route $ gsubRoute "static/" (const "")
      compile copyFileCompiler

    -- Track SCSS partials for dependency resolution (changes trigger style.scss recompile)
    match ("static/scss/_*.scss") $ compile getResourceBody

    -- Compile SCSS to CSS (using file path, not stdin, for proper @use support)
    match "static/scss/style.scss" $ do
      route $ constRoute "css/style.css"
      compile $ do
        -- Depend on all SCSS partials so changes trigger recompilation
        _ <- loadAll "static/scss/_*.scss" :: Compiler [Item String]
        path <- toFilePath <$> getUnderlying
        makeItem "" >>= withItemBody (const $ unixFilter "sass" ["--load-path=static/scss", "--style=compressed", path] "")

    -- Compile templates
    match "templates/*" $ compile templateBodyCompiler
    match "templates/partials/*" $ compile templateBodyCompiler

    -- Homepage (per language) - all languages available
    match "content/index.*.md" $ do
      route $ customRoute $ \ident ->
        let path = toFilePath ident
            lang = takeWhile (/= '.') $ drop 1 $ dropWhile (/= '.') $ takeFileName path
         in lang <> "/index.html"
      compile $ do
        lang <- getLang
        let langsCtx = homeLangsCtx cfg lang
            ctx = langsCtx <> langCtx lang <> siteCtx cfg lang
        getResourceBody
          >>= applyAsTemplate ctx
          >>= loadAndApplyTemplate "templates/home.html" ctx

    -- Blog posts
    match "content/posts/*/index.*.md" $ do
      route $ customRoute $ \ident ->
        let path = toFilePath ident
            parts = splitDirectories path
            slug = parts !! 2
            filename = parts !! 3
            lang = takeWhile (/= '.') $ drop 1 $ dropWhile (/= '.') $ filename
         in lang </> "posts" </> slug </> "index.html"
      compile $ do
        ident <- getUnderlying
        lang <- getLang
        langsCtx <- postLangsCtx cfg lang ident
        metadata <- getMetadata ident
        let enableMath = maybe False (== "true") $ lookupString "math" metadata
            enableMermaid = maybe False (== "true") $ lookupString "mermaid" metadata
            ctx = langsCtx <> postCtx cfg lang
        customPandocCompiler enableMath enableMermaid
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/post.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

    -- Post list pages
    create ["en/posts/index.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAllSnapshots "content/posts/*/index.en.md" "content"
        let langsCtx = postListLangsCtx cfg "en"
            ctx =
              constField "title" "Blog"
                <> listField "posts" (postCtx cfg "en") (pure posts)
                <> langsCtx
                <> langCtx "en"
                <> siteCtx cfg "en"
        makeItem ""
          >>= loadAndApplyTemplate "templates/post-list.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

    create ["zh/posts/index.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAllSnapshots "content/posts/*/index.zh.md" "content"
        let langsCtx = postListLangsCtx cfg "zh"
            ctx =
              constField "title" "博客"
                <> listField "posts" (postCtx cfg "zh") (pure posts)
                <> langsCtx
                <> langCtx "zh"
                <> siteCtx cfg "zh"
        makeItem ""
          >>= loadAndApplyTemplate "templates/post-list.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

    -- RSS feeds
    let feedCount = feedItemsCount $ feed cfg
    create ["en/feed.xml"] $ do
      route idRoute
      compile $ do
        posts <- fmap (take feedCount) . recentFirst =<< loadAllSnapshots "content/posts/*/index.en.md" "content"
        renderAtom (feedConfiguration cfg "en") feedCtx posts

    create ["zh/feed.xml"] $ do
      route idRoute
      compile $ do
        posts <- fmap (take feedCount) . recentFirst =<< loadAllSnapshots "content/posts/*/index.zh.md" "content"
        renderAtom (feedConfiguration cfg "zh") feedCtx posts

    -- Sitemap
    create ["sitemap.xml"] $ do
      route idRoute
      compile $ do
        enPosts <- loadAllSnapshots "content/posts/*/index.en.md" "content"
        zhPosts <- loadAllSnapshots "content/posts/*/index.zh.md" "content"
        let rootUrl = T.unpack $ baseUrl $ site cfg
            allPages = enPosts ++ zhPosts
            pageCtx = constField "root" rootUrl <> dateField "lastmod" "%Y-%m-%d" <> defaultContext
            sitemapCtx = listField "pages" pageCtx (pure allPages)
        makeItem ""
          >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

    -- 404 page
    match "content/404.html" $ do
      route $ constRoute "404.html"
      compile copyFileCompiler

-- | Get language from current item's file path
getLang :: Compiler String
getLang = do
  ident <- getUnderlying
  let path = toFilePath ident
      lang = takeWhile (/= '.') $ drop 1 $ dropWhile (/= '.') $ takeFileName path
  pure lang

-- | Homepage language context - all configured languages available
homeLangsCtx :: SiteConfig -> String -> Context String
homeLangsCtx cfg currentLang =
  availableLangsCtx $ map toLang $ languages cfg
  where
    toLang lang =
      AvailableLang
        { alCode = T.unpack $ langCode lang,
          alLabel = T.unpack $ langLabel lang,
          alUrl = "/" <> T.unpack (langCode lang) <> "/",
          alActive = T.unpack (langCode lang) == currentLang
        }

-- | Post list language context - all configured languages available
postListLangsCtx :: SiteConfig -> String -> Context String
postListLangsCtx cfg currentLang =
  availableLangsCtx $ map toLang $ languages cfg
  where
    toLang lang =
      AvailableLang
        { alCode = T.unpack $ langCode lang,
          alLabel = T.unpack $ langLabel lang,
          alUrl = "/" <> T.unpack (langCode lang) <> "/posts/",
          alActive = T.unpack (langCode lang) == currentLang
        }

-- | Post language context - only languages with available translations
postLangsCtx :: SiteConfig -> String -> Identifier -> Compiler (Context String)
postLangsCtx cfg currentLang ident = do
  let path = toFilePath ident
      parts = splitDirectories path
      slug = parts !! 2
  -- Check which language versions exist
  availableLangs <- filterM (hasTranslation slug) $ languages cfg
  pure $ availableLangsCtx $ map (toLang slug) availableLangs
  where
    toLang slug lang =
      AvailableLang
        { alCode = T.unpack $ langCode lang,
          alLabel = T.unpack $ langLabel lang,
          alUrl = "/" <> T.unpack (langCode lang) <> "/posts/" <> slug <> "/",
          alActive = T.unpack (langCode lang) == currentLang
        }
    hasTranslation slug lang = do
      let transPath = "content/posts" </> slug </> "index." <> T.unpack (langCode lang) <> ".md"
      getMatches (fromGlob transPath) >>= \matches -> pure (not $ null matches)
