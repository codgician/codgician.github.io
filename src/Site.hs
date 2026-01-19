{-# LANGUAGE OverloadedStrings #-}

module Site
  ( hakyllMain,
  )
where

import Hakyll

config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "_site",
      storeDirectory = "_cache",
      tmpDirectory = "_cache/tmp",
      providerDirectory = "."
    }

hakyllMain :: IO ()
hakyllMain = hakyllWith config $ do
  -- Copy static files
  match "static/**" $ do
    route $ gsubRoute "static/" (const "")
    compile copyFileCompiler

  -- Compile templates
  match "templates/*" $ compile templateBodyCompiler
  match "templates/partials/*" $ compile templateBodyCompiler
