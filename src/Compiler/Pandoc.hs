{-# LANGUAGE OverloadedStrings #-}

module Compiler.Pandoc
  ( customPandocCompiler,
    customPandocCompilerWithToc,
    slideCompiler,
    transformWithRenderers,
  )
where

import Compiler.RenderContext
import Compiler.Toc (generateToc)
import Content.Types (RenderFeatures (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Hakyll
import Skylighting (breezeDark)
import Text.Pandoc
import Text.Pandoc.Walk (walkM)
import Text.Read (readMaybe)

-- | Custom Pandoc compiler with injected render context
customPandocCompiler :: PandocRenderContext -> Compiler (Item String)
customPandocCompiler renderCtx =
  pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    (transform renderCtx)

-- | Custom Pandoc compiler that also returns TOC HTML
-- Returns (body, Maybe tocHtml)
customPandocCompilerWithToc :: PandocRenderContext -> Compiler (Item String, Maybe String)
customPandocCompilerWithToc renderCtx = do
  -- Read and parse the document
  body <- getResourceBody
  pandocDoc <- readPandocWith defaultHakyllReaderOptions body

  -- Transform the AST (math, mermaid)
  pandocDoc' <- transform renderCtx (itemBody pandocDoc)

  -- Generate TOC before writing
  let tocHtml = T.unpack <$> generateToc pandocDoc'

  -- Write HTML
  let htmlItem = writePandocWith defaultHakyllWriterOptions (pandocDoc' <$ pandocDoc)

  pure (htmlItem, tocHtml)

-- | Slide compiler using Pandoc's reveal.js writer
slideCompiler :: PandocRenderContext -> Compiler (Item String)
slideCompiler renderCtx = do
  metadata <- getUnderlying >>= getMetadata
  let slideLevel = fromMaybe 2 $ lookupString "slide-level" metadata >>= readMaybe

  -- Read raw content including YAML frontmatter (getResourceString doesn't strip it)
  body <- getResourceString

  -- Parse with Pandoc directly to preserve metadata
  pandocDoc <- unsafeCompiler $ do
    result <- runIO $ readMarkdown slideReaderOptions (T.pack $ itemBody body)
    case result of
      Left err -> fail $ "Pandoc read failed: " <> show err
      Right doc -> pure doc

  -- Apply transforms
  pandocDoc' <- transform renderCtx pandocDoc

  -- Write to reveal.js format with title slide template
  let writerOpts = slideWriterOptions slideLevel
  writeRevealJsWith writerOpts (Item (itemIdentifier body) pandocDoc')

-- | Minimal template that outputs title slide + body for reveal.js
revealJsBodyTemplate :: Text
revealJsBodyTemplate =
  T.unlines
    [ "$if(title)$",
      "<section id=\"title-slide\">",
      "  <h1 class=\"title\">$title$</h1>",
      "$if(subtitle)$",
      "  <p class=\"subtitle\">$subtitle$</p>",
      "$endif$",
      "$for(author)$",
      "  <p class=\"author\">$author$</p>",
      "$endfor$",
      "$if(date)$",
      "  <p class=\"date\">$date$</p>",
      "$endif$",
      "</section>",
      "$endif$",
      "$body$"
    ]

-- | Custom reveal.js writer for Hakyll using minimal template
writeRevealJsWith :: WriterOptions -> Item Pandoc -> Compiler (Item String)
writeRevealJsWith wopt (Item itemi doc) = unsafeCompiler $ do
  -- Compile template in IO context
  tplResult <- compileTemplate "" revealJsBodyTemplate
  case tplResult of
    Left err -> fail $ "Template parsing failed: " <> err
    Right tpl -> do
      result <- runIO $ writeRevealJs wopt {writerTemplate = Just tpl} doc
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
      writerHighlightStyle = Just breezeDark,
      writerExtensions =
        writerExtensions defaultHakyllWriterOptions
          <> extensionsFromList
            [ Ext_native_divs,
              Ext_native_spans,
              Ext_fenced_divs,
              Ext_bracketed_spans
            ]
    }

-- | Transform Pandoc AST via Hakyll Compiler (wraps IO transform)
transform :: PandocRenderContext -> Pandoc -> Compiler Pandoc
transform renderCtx = unsafeCompiler . transformWithRenderers renderCtx

-- | Transform Pandoc AST using injected renderers (pure IO, testable)
transformWithRenderers :: PandocRenderContext -> Pandoc -> IO Pandoc
transformWithRenderers renderCtx doc = do
  doc' <- if renderMath features then walkM (transformMath renderers) doc else pure doc
  doc'' <- if renderMermaid features then walkM (transformMermaid renderers) doc' else pure doc'
  if renderTikZ features then walkM (transformTikZ renderers) doc'' else pure doc''
  where
    features = pandocRenderFeatures renderCtx
    renderers = pandocRenderers renderCtx

-- | Transform Math to rendered HTML
transformMath :: PandocRenderers -> Inline -> IO Inline
transformMath renderers (Math mathType content) = do
  rendered <- pandocMathHtml renderers mathType content
  pure $ RawInline (Format "html") rendered
transformMath _ x = pure x

-- | Transform Mermaid code blocks to SVG (dual theme: light + dark)
transformMermaid :: PandocRenderers -> Block -> IO Block
transformMermaid renderers (CodeBlock (_, classes, _) content)
  | "mermaid" `elem` classes = do
      html <- pandocMermaidHtml renderers content
      pure $
        RawBlock (Format "html") $
          "<div class=\"mermaid\">" <> html <> "</div>"
transformMermaid _ x = pure x

-- | Transform TikZ code blocks to inline SVG.
transformTikZ :: PandocRenderers -> Block -> IO Block
transformTikZ renderers (CodeBlock (_, classes, _) content)
  | "tikz" `elem` classes = do
      html <- pandocTikZHtml renderers content
      pure $ RawBlock (Format "html") html
transformTikZ _ x = pure x
