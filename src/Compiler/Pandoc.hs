{-# LANGUAGE OverloadedStrings #-}

module Compiler.Pandoc
  ( customPandocCompiler,
  )
where

import Compiler.KaTeX (cachedKaTeX)
import Compiler.Mermaid (cachedMermaid)
import Data.Text (Text)
import qualified Data.Text as T
import Hakyll
import Text.Pandoc
import Text.Pandoc.Walk (walkM)
import qualified Data.Version as V
import qualified Paths_builder as Meta

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
  rendered <- cachedKaTeX filterVersion displayMode (T.pack content)
  pure $ RawInline (Format "html") (T.unpack rendered)
transformMath x = pure x

-- | Transform Mermaid code blocks to SVG
transformMermaid :: Block -> IO Block
transformMermaid (CodeBlock (ident, classes, attrs) content)
  | "mermaid" `elem` classes = do
      svg <- cachedMermaid filterVersion (T.pack content)
      pure $ RawBlock (Format "html") $ T.unpack $
        "<div class=\"mermaid\">" <> svg <> "</div>"
transformMermaid x = pure x
