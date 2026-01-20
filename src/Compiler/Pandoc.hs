{-# LANGUAGE OverloadedStrings #-}

module Compiler.Pandoc
  ( customPandocCompiler,
    slideCompiler,
  )
where

import Compiler.KaTeX (cachedKaTeX)
import Compiler.Mermaid (cachedMermaid)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Version as V
import Hakyll
import qualified Paths_builder as Meta
import Text.Pandoc
import Text.Pandoc.Highlighting (Style, pygments)
import Text.Pandoc.Walk (walkM)

-- | Get filter version from Cabal
filterVersion :: Text
filterVersion = T.pack $ V.showVersion Meta.version

-- | Custom Pandoc compiler with KaTeX and Mermaid transforms
customPandocCompiler :: Bool -> Bool -> Compiler (Item String)
customPandocCompiler enableMath enableMermaid =
  pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    (transform enableMath enableMermaid)

-- | Slide compiler using Pandoc's reveal.js writer
slideCompiler :: Bool -> Compiler (Item String)
slideCompiler enableMath = do
  -- Get metadata for slide configuration
  metadata <- getUnderlying >>= getMetadata
  let slideLevel = maybe 2 read $ lookupString "slide-level" metadata
  
  -- Read raw content including YAML frontmatter (getResourceString doesn't strip it)
  body <- getResourceString
  
  -- Parse with Pandoc directly to preserve metadata
  pandocDoc <- unsafeCompiler $ do
    result <- runIO $ readMarkdown slideReaderOptions (T.pack $ itemBody body)
    case result of
      Left err -> fail $ "Pandoc read failed: " <> show err
      Right doc -> pure doc
  
  -- Apply math transform
  pandocDoc' <- transform enableMath False pandocDoc
  
  -- Write to reveal.js format with title slide template
  let writerOpts = slideWriterOptions slideLevel
  writeRevealJsWith writerOpts (Item (itemIdentifier body) pandocDoc')

-- | Minimal template that outputs title slide + body for reveal.js
revealJsBodyTemplate :: Text
revealJsBodyTemplate = T.unlines
  [ "$if(title)$"
  , "<section id=\"title-slide\">"
  , "  <h1 class=\"title\">$title$</h1>"
  , "$if(subtitle)$"
  , "  <p class=\"subtitle\">$subtitle$</p>"
  , "$endif$"
  , "$for(author)$"
  , "  <p class=\"author\">$author$</p>"
  , "$endfor$"
  , "$if(date)$"
  , "  <p class=\"date\">$date$</p>"
  , "$endif$"
  , "</section>"
  , "$endif$"
  , "$body$"
  ]

-- | Custom reveal.js writer for Hakyll using minimal template
writeRevealJsWith :: WriterOptions -> Item Pandoc -> Compiler (Item String)
writeRevealJsWith wopt (Item itemi doc) = unsafeCompiler $ do
  -- Compile template in IO context
  tplResult <- compileTemplate "" revealJsBodyTemplate
  case tplResult of
    Left err -> fail $ "Template parsing failed: " <> err
    Right tpl -> do
      result <- runIO $ writeRevealJs wopt { writerTemplate = Just tpl } doc
      case result of
        Left err -> fail $ "Pandoc reveal.js writer failed: " <> show err
        Right output -> pure $ Item itemi $ T.unpack output

-- | Reader options for slides
slideReaderOptions :: ReaderOptions
slideReaderOptions =
  defaultHakyllReaderOptions
    { readerExtensions =
        readerExtensions defaultHakyllReaderOptions
          <> extensionsFromList
            [ Ext_fenced_divs,
              Ext_bracketed_spans,
              Ext_native_divs,
              Ext_native_spans,
              Ext_fenced_code_attributes,
              Ext_implicit_figures,
              Ext_smart,
              Ext_footnotes
            ]
    }

-- | Writer options for reveal.js slides
slideWriterOptions :: Int -> WriterOptions
slideWriterOptions slideLevel =
  defaultHakyllWriterOptions
    { writerSlideLevel = Just slideLevel,
      writerSectionDivs = True,
      writerHighlightStyle = Just pygments,
      writerExtensions =
        writerExtensions defaultHakyllWriterOptions
          <> extensionsFromList 
            [ Ext_native_divs, 
              Ext_native_spans,
              Ext_fenced_divs,
              Ext_bracketed_spans
            ]
    }

-- | Transform Pandoc AST
transform :: Bool -> Bool -> Pandoc -> Compiler Pandoc
transform enableMath enableMermaid doc = unsafeCompiler $ do
  doc' <- if enableMath then walkM transformMath doc else pure doc
  if enableMermaid then walkM transformMermaid doc' else pure doc'

-- | Transform Math to rendered HTML
transformMath :: Inline -> IO Inline
transformMath (Math mathType content) = do
  let displayMode = case mathType of
        DisplayMath -> True
        InlineMath -> False
  rendered <- cachedKaTeX filterVersion displayMode content
  pure $ RawInline (Format "html") rendered
transformMath x = pure x

-- | Transform Mermaid code blocks to SVG
transformMermaid :: Block -> IO Block
transformMermaid (CodeBlock (_, classes, _) content)
  | "mermaid" `elem` classes = do
      svg <- cachedMermaid filterVersion content
      pure $ RawBlock (Format "html") $
        "<div class=\"mermaid\">" <> svg <> "</div>"
transformMermaid x = pure x
