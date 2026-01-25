---
name: fact-haskell-patterns
description: "FACT: Haskell idioms and patterns specific to this codebase - types, imports, error handling."
type: fact
---

# Haskell Patterns for This Project

> Language-level facts specific to how Haskell is used in this codebase.

## Type Conventions

| Use | Don't Use | Boundary Exception |
|-----|-----------|-------------------|
| `Text` | `String` | Hakyll APIs require `String` |
| `Maybe a` | `fromJust` | Never |
| `Either e a` | `error` | Only in env var checks |
| Explicit type signatures | Type inference only | Always annotate top-level |

## Import Style

```haskell
-- Qualified imports for common clashes
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

-- Explicit imports for everything else
import Data.Maybe (fromMaybe, listToMaybe)
import Control.Monad (when, unless, forM_)
```

## Error Handling Hierarchy

```
Preferred (most → least):
1. Maybe a          -- Simple absence
2. Either Text a    -- Recoverable with message
3. MonadFail m      -- In monadic context
4. error "msg"      -- ONLY for "impossible" states (env vars)
```

## Hakyll-Specific Patterns

### Context Composition
```haskell
-- Specific → General → Default
myContext = 
    specificField <>
    sharedFields <>
    defaultContext
```

### Route Patterns
```haskell
-- Always use explicit language in route
route $ customRoute $ \ident ->
    lang </> section </> slug </> "index.html"
```

### Compiler Patterns
```haskell
-- Snapshot for reuse
compile $ do
    pandocCompiler
        >>= saveSnapshot "content"  -- For feeds, lists
        >>= loadAndApplyTemplate "templates/post.html" ctx
```

## Common Helpers

These exist in the codebase:

| Helper | Location | Purpose |
|--------|----------|---------|
| `slugFromPath` | `src/Site.hs` | Extract slug from identifier |
| `langStr` | `src/Site.hs` | Language code as String |
| `safeInit` | `src/Site.hs` | Safe `init` (no error on []) |
| `nubOrd` | `src/Site.hs` | O(n log n) deduplication |

## Anti-Patterns in This Codebase

| Anti-Pattern | Why Bad Here | Better |
|--------------|--------------|--------|
| `head`, `tail` | Partial functions | Pattern match or `listToMaybe` |
| `nub` | O(n²) | Use `nubOrd` |
| `read` | Partial | Use `readMaybe` |
| Wildcard imports | Unclear dependencies | Explicit imports |
| `String` concatenation | Inefficient | Use `Text` with `<>` |

## Module Organization

```
src/
├── Site.hs          # Main rules, routes
├── Config.hs        # YAML config types
├── Context.hs       # Template contexts
├── Paginate.hs      # Pagination helpers
├── Feed.hs          # RSS generation
└── Compiler/
    ├── Pandoc.hs    # Markdown → HTML
    ├── KaTeX.hs     # Math rendering
    ├── Mermaid.hs   # Diagram rendering
    └── Cache.hs     # Content-addressed caching
```

## GHC Warnings Active

This project uses strict warnings:

```yaml
ghc-options:
  - -Werror                    # All warnings are errors
  - -Wall                      # Standard warnings
  - -Wunused-packages          # Unused dependencies
  - -Wmissing-export-lists     # Must export explicitly
  # ... (see package.yaml for full list)
```

**Implication**: Code that compiles has passed all these checks.
