{-# LANGUAGE OverloadedStrings #-}

module Site
  ( hakyllMain,
  )
where

import Config (loadConfig)
import Context (langCtx, siteCtx)
import Hakyll
import System.FilePath (takeFileName)

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

getLang :: Compiler String
getLang = do
  ident <- getUnderlying
  let path = toFilePath ident
      lang = takeWhile (/= '.') $ drop 1 $ dropWhile (/= '.') $ takeFileName path
  pure lang
