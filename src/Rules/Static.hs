{-# LANGUAGE OverloadedStrings #-}

-- | Static asset and template rules for the Hakyll site.
--
-- Handles: static file copying, SCSS compilation, syntax highlighting CSS,
-- template compilation, error pages, and content-directory image assets.
module Rules.Static
  ( staticRules,
    contentAssetRules,
  )
where

import Config (Language (..), SiteConfig (..))
import Content.Types (Section (..), langCodeString, sectionPathString)
import Control.Monad (forM_)
import Data.List (isPrefixOf)
import Hakyll
import Routes (contentAssetOutputPath, routeErrorMessage, slugFromSectionIdentifier)
import Skylighting (Style, backgroundColor, breezeDark, defaultColor, fromColor, kate, styleToCss)
import System.FilePath (takeFileName)

-- ============================================================================
-- Public API
-- ============================================================================

-- | All static rules that do not depend on site config.
staticRules :: Rules ()
staticRules = do
  staticFiles
  scssCompilation
  syntaxHighlightingCss
  templates
  errorPage

-- | Content asset rules (image copying from content directories).
-- Depends on site config for language list.
contentAssetRules :: SiteConfig -> Rules ()
contentAssetRules = contentAssets

-- ============================================================================
-- Static Files
-- ============================================================================

staticFiles :: Rules ()
staticFiles =
  match ("static/**" .&&. complement "static/scss/**") $ do
    route $ gsubRoute "static/" (const "")
    compile copyFileCompiler

-- ============================================================================
-- Content Assets (images in post/slide directories)
-- ============================================================================

imageExtensions :: [String]
imageExtensions = ["jpg", "png", "gif", "svg", "webp"]

-- | Copy images from content directories to lang-prefixed output paths.
contentAssets :: SiteConfig -> Rules ()
contentAssets cfg =
  forM_ imageExtensions $ \ext ->
    forM_ [Posts, Slides] $ \sectionValue ->
      match (fromGlob $ "content/" <> sectionPathString sectionValue <> "/*/*." <> ext) $
        forM_ (languages cfg) $ \lang ->
          version (langCodeString $ langCode lang) $ do
            route $ customRoute $ \identifier ->
              case slugFromSectionIdentifier sectionValue identifier of
                Right slug ->
                  contentAssetOutputPath (langCode lang) sectionValue slug (takeFileName $ toFilePath identifier)
                Left err -> error $ routeErrorMessage err
            compile copyFileCompiler

-- ============================================================================
-- SCSS Compilation
-- ============================================================================

scssCompilation :: Rules ()
scssCompilation = do
  match "static/scss/_*.scss" $ compile getResourceBody
  match "static/scss/style.scss" $ do
    route $ constRoute "css/style.css"
    compile $ do
      _ <- loadAll "static/scss/_*.scss" :: Compiler [Item String]
      path <- toFilePath <$> getUnderlying
      makeItem ("" :: String)
        >>= withItemBody
          (const $ unixFilter "sass" ["--load-path=static/scss", "--style=compressed", path] "")

-- ============================================================================
-- Syntax Highlighting CSS
-- ============================================================================

syntaxHighlightingCss :: Rules ()
syntaxHighlightingCss = create ["css/syntax.css"] $ do
  route idRoute
  compile $ makeItem syntaxCssWithThemes

syntaxCssWithThemes :: String
syntaxCssWithThemes =
  unlines
    [ "/* Light theme (kate) - default */",
      styleToCss kate,
      "/* Dark theme (breezeDark) - applied via system preference or manual override */",
      "@media (prefers-color-scheme: dark) {",
      "  :root:not([data-theme=\"light\"]) div.sourceCode { " ++ darkBgCss ++ " }",
      extractTokenStyles breezeDark ":root:not([data-theme=\"light\"])",
      "}",
      ":root[data-theme=\"dark\"] div.sourceCode { " ++ darkBgCss ++ " }",
      extractTokenStyles breezeDark ":root[data-theme=\"dark\"]"
    ]
  where
    darkBgCss = bgColorCss breezeDark ++ fgColorCss breezeDark

bgColorCss :: Style -> String
bgColorCss style = maybe "" (\c -> "background-color: " ++ fromColor c ++ "; ") (backgroundColor style)

fgColorCss :: Style -> String
fgColorCss style = maybe "" (\c -> "color: " ++ fromColor c ++ "; ") (defaultColor style)

extractTokenStyles :: Style -> String -> String
extractTokenStyles style prefix =
  unlines
    [ prefix ++ " " ++ line
    | line <- lines (styleToCss style),
      "code span" `isPrefixOf` line
    ]

-- ============================================================================
-- Templates
-- ============================================================================

templates :: Rules ()
templates = do
  match "templates/*" $ compile templateBodyCompiler
  match "templates/partials/*" $ compile templateBodyCompiler

-- ============================================================================
-- Error Page
-- ============================================================================

errorPage :: Rules ()
errorPage = match "content/404.html" $ do
  route $ constRoute "404.html"
  compile copyFileCompiler
