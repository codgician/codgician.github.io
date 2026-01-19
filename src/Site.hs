{-# LANGUAGE OverloadedStrings #-}

module Site
  ( hakyllMain,
  )
where

import Compiler.Pandoc (customPandocCompiler)
import Config (loadConfig)
import Context (langCtx, postCtx, siteCtx)
import Feed (feedConfiguration, feedCtx)
import Hakyll
import System.FilePath (splitDirectories, takeFileName, (</>))

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

    -- Compile SCSS to CSS
    match "static/scss/style.scss" $ do
      route $ constRoute "css/style.css"
      compile $
        getResourceString
          >>= withItemBody (unixFilter "sass" ["--stdin", "--style=compressed"])

    -- Compile templates
    match "templates/*" $ compile templateBodyCompiler
    match "templates/partials/*" $ compile templateBodyCompiler

    -- Homepage (per language)
    match "content/index.*.md" $ do
      route $ customRoute $ \ident ->
        let path = toFilePath ident
            lang = takeWhile (/= '.') $ drop 1 $ dropWhile (/= '.') $ takeFileName path
         in lang <> "/index.html"
      compile $ do
        lang <- getLang
        getResourceBody
          >>= applyAsTemplate (langCtx lang <> siteCtx cfg)
          >>= loadAndApplyTemplate "templates/home.html" (langCtx lang <> siteCtx cfg)

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
        lang <- getLang
        metadata <- getMetadata =<< getUnderlying
        let enableMath = maybe False (== "true") $ lookupString "math" metadata
            enableMermaid = maybe False (== "true") $ lookupString "mermaid" metadata
        customPandocCompiler enableMath enableMermaid
          >>= loadAndApplyTemplate "templates/post.html" (postCtx cfg lang)
          >>= loadAndApplyTemplate "templates/default.html" (postCtx cfg lang)
          >>= relativizeUrls

    -- Post list pages
    create ["en/posts/index.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "content/posts/*/index.en.md"
        let ctx =
              constField "title" "Blog"
                <> listField "posts" (postCtx cfg "en") (pure posts)
                <> langCtx "en"
                <> siteCtx cfg
        makeItem ""
          >>= loadAndApplyTemplate "templates/post-list.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

    create ["zh/posts/index.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "content/posts/*/index.zh.md"
        let ctx =
              constField "title" "博客"
                <> listField "posts" (postCtx cfg "zh") (pure posts)
                <> langCtx "zh"
                <> siteCtx cfg
        makeItem ""
          >>= loadAndApplyTemplate "templates/post-list.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

    -- RSS feeds
    create ["en/feed.xml"] $ do
      route idRoute
      compile $ do
        posts <- fmap (take 20) . recentFirst =<< loadAll "content/posts/*/index.en.md"
        renderAtom (feedConfiguration cfg "en") feedCtx posts

    create ["zh/feed.xml"] $ do
      route idRoute
      compile $ do
        posts <- fmap (take 20) . recentFirst =<< loadAll "content/posts/*/index.zh.md"
        renderAtom (feedConfiguration cfg "zh") feedCtx posts

    -- 404 page
    match "content/404.html" $ do
      route $ constRoute "404.html"
      compile copyFileCompiler

getLang :: Compiler String
getLang = do
  ident <- getUnderlying
  let path = toFilePath ident
      lang = takeWhile (/= '.') $ drop 1 $ dropWhile (/= '.') $ takeFileName path
  pure lang
