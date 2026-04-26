{-# LANGUAGE OverloadedStrings #-}

-- | Generic pagination support for bilingual Hakyll sites.
--
-- Design principles:
-- 1. Language-agnostic core: pagination logic doesn't know about languages
-- 2. Bilingual support via per-language Paginate instances
-- 3. Clean URLs: first page at /{lang}/posts/, subsequent at /{lang}/posts/page/2/
-- 4. Graceful degradation: single-page lists hide pagination controls
module Paginate
  ( -- * Page ID Generators
    makePageId,

    -- * Context Builders
    paginationCtx,
  )
where

import Content.Types (LangCode, Section)
import qualified Data.Map as Map
import Hakyll
import Routes (sectionPageIdentifier)

-- | Generate page identifiers with clean URLs.
--
-- Page 1: /{lang}/{section}/index.html
-- Page 2+: /{lang}/{section}/page/{n}/index.html
--
-- Example:
-- @
-- makePageId (LangCode "en") Posts 1  -- "en/posts/index.html"
-- makePageId (LangCode "en") Posts 2  -- "en/posts/page/2/index.html"
-- makePageId (LangCode "zh") Slides 3 -- "zh/slides/page/3/index.html"
-- @
makePageId :: LangCode -> Section -> PageNumber -> Identifier
makePageId = sectionPageIdentifier

-- | Pagination context with hasPagination boolean for templates.
--
-- Wraps Hakyll's paginateContext and adds:
-- - hasPagination: Boolean, true if more than one page
--
-- Provides all standard pagination fields:
-- - firstPageNum, firstPageUrl
-- - previousPageNum, previousPageUrl
-- - nextPageNum, nextPageUrl
-- - lastPageNum, lastPageUrl
-- - currentPageNum, currentPageUrl
-- - numPages
paginationCtx :: Paginate -> PageNumber -> Context a
paginationCtx pag currentPage =
  paginateContext pag currentPage
    <> boolField "hasPagination" (const $ numPages > 1)
  where
    numPages = Map.size $ paginateMap pag
