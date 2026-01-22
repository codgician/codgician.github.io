# Haskell Coding Standard

## Type Signatures

Every top-level function must have a type signature:

```haskell
-- ✅ GOOD
myFunction :: Text -> Compiler String
myFunction input = ...

-- ❌ BAD (missing signature)
myFunction input = ...
```

## Text vs String

Use `Text` by default. Only use `String` when required by Hakyll APIs:

```haskell
-- ✅ GOOD
import Data.Text (Text)
import qualified Data.Text as T

processContent :: Text -> Text
processContent = T.toUpper

-- When Hakyll requires String
toFilePath :: Identifier -> String  -- Hakyll API, must use String

-- ❌ BAD
processContent :: String -> String  -- Should be Text
```

## Imports

Use explicit imports. Qualify where names are ambiguous:

```haskell
-- ✅ GOOD
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe, catMaybes)
import Control.Monad (forM_, filterM)

-- ❌ BAD
import Data.Text  -- Importing everything
import Data.Maybe  -- Importing everything
```

## Context Composition

Order: specific → shared → default (first match wins):

```haskell
-- ✅ GOOD
let ctx = constField "customField" value
       <> postCtx cfg lang
       <> defaultContext

-- ❌ BAD (wrong order - default shadows specific)
let ctx = defaultContext
       <> postCtx cfg lang
       <> constField "customField" value
```

## Helper Extraction

Extract helper when pattern appears 3+ times:

```haskell
-- ✅ GOOD: Extracted helper
extractLang :: Identifier -> String
extractLang i = takeWhile (/= '.') $ drop 1 $ dropWhile (/= '.') $ takeFileName $ toFilePath i

-- Used in multiple places
currentLang = extractLang <$> getUnderlying
langFromItem item = extractLang (itemIdentifier item)

-- ❌ BAD: Copy-pasted inline
route1 = ... takeWhile (/= '.') $ drop 1 $ dropWhile (/= '.') $ takeFileName ...
route2 = ... takeWhile (/= '.') $ drop 1 $ dropWhile (/= '.') $ takeFileName ...
```

## Error Handling

Prefer `Maybe`/`Either` over `error`:

```haskell
-- ✅ GOOD
findPost :: String -> Compiler (Maybe Identifier)
findPost slug = do
  matches <- getMatches pattern
  case matches of
    (i:_) -> pure (Just i)
    [] -> pure Nothing

-- ❌ BAD
findPost :: String -> Compiler Identifier
findPost slug = do
  matches <- getMatches pattern
  case matches of
    (i:_) -> pure i
    [] -> error "Post not found"  -- Crashes at runtime
```

## Pattern Matching

Prefer pattern matching over boolean checks:

```haskell
-- ✅ GOOD
case mValue of
  Just v -> process v
  Nothing -> defaultValue

-- ❌ BAD
if isJust mValue
  then process (fromJust mValue)  -- Partial function
  else defaultValue
```

## Monadic Composition

Use `>>=` or `do` notation consistently:

```haskell
-- ✅ GOOD: Clear pipeline
compile = customPandocCompiler enableMath enableMermaid
  >>= saveSnapshot "content"
  >>= loadAndApplyTemplate "templates/post.html" ctx
  >>= loadAndApplyTemplate "templates/default.html" ctx
  >>= relativizeUrls

-- ✅ ALSO GOOD: do notation for complex logic
compile = do
  (enableMath, enableMermaid) <- getFeatureFlags
  item <- customPandocCompiler enableMath enableMermaid
  ...

-- ❌ BAD: Mixing styles inconsistently
compile = do
  item <- customPandocCompiler a b
  item' <- return item >>= saveSnapshot "content"  -- Unnecessary
```

## Record Syntax

Use record syntax for data types with multiple fields:

```haskell
-- ✅ GOOD
data SitemapEntry = SitemapEntry
  { sitemapLoc :: String
  , sitemapPriority :: String
  }

-- ❌ BAD (positional for complex types)
data SitemapEntry = SitemapEntry String String
```

## Comments

Document non-obvious code, not obvious code:

```haskell
-- ✅ GOOD: Explains WHY
-- | Create fallback posts for missing translations.
-- Posts without a native version in the target language
-- will be served from the best available translation.
createFallbackPost :: ...

-- ❌ BAD: States the obvious
-- | Adds two numbers together
add :: Int -> Int -> Int
add x y = x + y
```

## Module Organization

```haskell
-- 1. Language extensions
{-# LANGUAGE OverloadedStrings #-}

-- 2. Module declaration
module Site (hakyllMain) where

-- 3. Imports (grouped: base → external → internal)
import Control.Monad (forM_)
import Data.Text (Text)
import Hakyll
import Config  -- Internal modules

-- 4. Constants/configuration

-- 5. Main entry point

-- 6. Rule definitions (logical grouping with headers)

-- 7. Helper functions
```

## External Tool Integration

Always use subprocess, never FFI or Haskell bindings:

```haskell
-- ✅ GOOD: Simple subprocess
renderKaTeX :: Bool -> Text -> IO Text
renderKaTeX displayMode content = do
  let args = ["--strict"] ++ ["--display-mode" | displayMode]
  T.pack <$> readProcess "katex" args (T.unpack content)

-- ❌ BAD: Complex Haskell integration (inline-js, ghcjs, etc.)
```

## Content-Addressed Caching

Include all version components in cache key:

```haskell
-- ✅ GOOD: Complete cache key
cachedRender :: Text -> Text -> Text -> Text -> (Text -> IO Text) -> IO Text
cachedRender toolName toolVersion toolOptions content render = do
  filterVersion <- getFilterVersion  -- from Cabal-generated Paths module
  let key = sha256 $ T.intercalate "\n"
        ["v1", toolName, toolVersion, toolOptions, filterVersion, content]
      path = "_artifacts" </> T.unpack (T.take 32 key) <.> "html"
  exists <- doesFileExist path
  if exists 
    then T.readFile path
    else do
      result <- render content
      T.writeFile path result
      pure result

-- ❌ BAD: Missing version components, MVar tracking, automatic GC
```

## Metadata Helpers

Standard pattern for accessing frontmatter:

```haskell
metaStr :: String -> Metadata -> String
metaStr k = fromMaybe "" . lookupString k

metaBool :: String -> Metadata -> Bool
metaBool k m = maybe False ((== "true") . map toLower) $ lookupString k m

boolCtx :: String -> Metadata -> Context String
boolCtx k m = if metaBool k m then constField k "true" else mempty
```

## Template Application

Standard pipeline for page compilation:

```haskell
item >>= loadAndApplyTemplate "templates/specific.html" ctx
     >>= loadAndApplyTemplate "templates/default.html" ctx
     >>= relativizeUrls
```
