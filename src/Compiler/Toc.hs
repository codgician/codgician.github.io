{-# LANGUAGE OverloadedStrings #-}

-- | Table of Contents generation from Pandoc AST.
--
-- Extracts headings from a Pandoc document and generates a nested HTML list.
-- The TOC links to heading IDs that Pandoc auto-generates.
module Compiler.Toc
  ( generateToc,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Definition (Block (..), Pandoc (..))
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Walk (query)

-- | A single TOC entry with level, ID, and title
data TocEntry = TocEntry
  { tocLevel :: Int,
    tocId :: Text,
    tocTitle :: Text
  }
  deriving (Show)

-- | Extract all headings from a Pandoc document
-- Filters out headings with empty IDs (can't link to them)
extractHeadings :: Pandoc -> [TocEntry]
extractHeadings = filter (not . T.null . tocId) . query getHeading
  where
    getHeading :: Block -> [TocEntry]
    getHeading (Header level (headingId, _, _) inlines) =
      [TocEntry level headingId (stringify inlines)]
    getHeading _ = []

-- | Generate TOC HTML from a Pandoc document
-- Returns Nothing if there are no headings, Just html otherwise
generateToc :: Pandoc -> Maybe Text
generateToc doc =
  let headings = extractHeadings doc
      -- Only include h2 and h3 for cleaner TOC
      filtered = filter (\h -> tocLevel h >= 2 && tocLevel h <= 3) headings
   in if null filtered
        then Nothing
        else Just $ renderToc filtered

-- | Render TOC entries as nested HTML list
renderToc :: [TocEntry] -> Text
renderToc entries = "<ul class=\"toc-list\">" <> renderEntries entries <> "</ul>"
  where
    renderEntries :: [TocEntry] -> Text
    renderEntries [] = ""
    renderEntries (e : es) =
      let (children, rest) = span (\x -> tocLevel x > tocLevel e) es
          childrenHtml =
            if null children
              then ""
              else "<ul>" <> renderEntries children <> "</ul>"
       in "<li>"
            <> "<a href=\"#"
            <> escapeHtml (tocId e)
            <> "\">"
            <> escapeHtml (tocTitle e)
            <> "</a>"
            <> childrenHtml
            <> "</li>"
            <> renderEntries rest

-- | Basic HTML escaping (& must be escaped first to avoid double-escaping)
escapeHtml :: Text -> Text
escapeHtml =
  T.replace "<" "&lt;"
    . T.replace ">" "&gt;"
    . T.replace "\"" "&quot;"
    . T.replace "&" "&amp;"
