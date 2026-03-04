# AI Agent Guidelines

Essential context for AI coding assistants working on this repository.

## Project Overview

A minimalist personal website and blog built with **Hakyll** (Haskell static
site generator) and **Nix** (reproducible builds). Supports bilingual content
(English/Chinese).

**Tech Stack**: Haskell, Hakyll, Nix, SCSS, Pandoc, KaTeX, Mermaid

## Quick Commands

```bash
nix build              # Build site (MUST pass before claiming done)
nix run . -- watch     # Dev server at http://127.0.0.1:8000
nix develop            # Dev shell with HLS, cabal, ormolu
nix fmt                # Format all code
```

## Repository Structure

```
.
├── src/                    # Haskell source
│   ├── Site.hs             # Main Hakyll rules
│   ├── Config.hs           # YAML config loader
│   ├── Context.hs          # Template context fields
│   ├── Paginate.hs         # Pagination support
│   └── Compiler/           # Pandoc, KaTeX, Mermaid, Cache
├── content/                # Markdown content
│   ├── posts/{slug}/       # Blog posts (index.en.md, index.zh.md)
│   ├── slides/{slug}/      # Reveal.js slides
│   └── index.{lang}.md     # Homepage per language
├── templates/              # Hakyll HTML templates
├── static/scss/            # SCSS stylesheets
├── config.yaml             # Site configuration
└── flake.nix               # Nix build system
```

---

## Core Principles

### Philosophy

1. **Simplicity over Cleverness** - If it feels clever, it's probably wrong
2. **Correctness over Performance** - Fresh builds > fragile caching
3. **Nix is Truth** - All dependencies in `flake.nix`, nothing else
4. **External Tools via Subprocess** - KaTeX/Mermaid via CLI, never FFI

### Non-Negotiable Constraints

| Constraint            | Implication                                     |
| --------------------- | ----------------------------------------------- |
| **Bilingual (en/zh)** | Every content page needs both language versions |
| **Minimalist**        | No unnecessary features, delete before adding   |
| **Nix-managed**       | All dependencies via `flake.nix` only           |
| **Build must pass**   | `nix build` is the single source of truth       |

### Decision Table

| Situation         | Do This                            | Not This                |
| ----------------- | ---------------------------------- | ----------------------- |
| Add external tool | Subprocess via `readProcess`       | FFI or Haskell binding  |
| Need caching      | Content-addressed in `_artifacts/` | Persist `_cache/` in CI |
| Template logic    | Hakyll built-in `$if$`/`$for$`     | Complex templating      |
| New dependency    | Add to `flake.nix`                 | Manual install          |
| Unused feature    | Delete it                          | Keep "for later"        |

---

## Haskell Conventions

### Type Safety

| Use                      | Don't Use                     |
| ------------------------ | ----------------------------- |
| `Text`                   | `String` (except Hakyll APIs) |
| `Maybe a`                | `fromJust`                    |
| `Either e a`             | `error` (except env vars)     |
| Explicit type signatures | Type inference only           |

### Import Style

```haskell
-- Qualified for common clashes
import qualified Data.Text as T
import qualified Data.Map as Map

-- Explicit for everything else
import Data.Maybe (fromMaybe, listToMaybe)
import Control.Monad (forM_, filterM)
```

### Context Composition

Order: specific → shared → default (first match wins):

```haskell
let ctx = constField "custom" value  -- specific
       <> postCtx cfg lang           -- shared
       <> defaultContext             -- default
```

### Anti-Patterns

| Don't                      | Better                         |
| -------------------------- | ------------------------------ |
| `fromJust`, `head`, `tail` | Pattern match or `listToMaybe` |
| `error "msg"`              | Return `Maybe`/`Either`        |
| Wildcard imports           | Explicit imports               |
| Copy-paste code            | Extract helper (3+ uses)       |
| `nub`                      | `nubOrd` (O(n log n))          |

### Existing Helpers

| Helper         | Purpose                        |
| -------------- | ------------------------------ |
| `slugFromPath` | Extract slug from identifier   |
| `extractLang`  | Get language from filename     |
| `langStr`      | Language code as String        |
| `safeInit`     | Safe `init` (no error on [])   |
| `nubOrd`       | O(n log n) deduplication       |
| `transStr`     | Get translated string directly |

---

## SCSS Conventions

### Variables Only

```scss
// GOOD
.card {
  padding: $space-lg;
  background: var(--color-surface);
}

// BAD - hardcoded values
.card {
  padding: 24px;
  background: #f7f3e3;
}
```

### Color System

All colors via CSS variables for theming:

- `--color-bg`, `--color-text`, `--color-primary`
- `--color-secondary`, `--color-surface`, `--color-border`

### Spacing Scale

`$space-xs` (4px) → `$space-sm` (8px) → `$space-md` (16px) → `$space-lg` (24px)
→ `$space-xl` (32px)

---

## URL Structure

| Content Type | Pattern                    | Example                  |
| ------------ | -------------------------- | ------------------------ |
| Post         | `/:lang/posts/:slug/`      | `/en/posts/hello-world/` |
| Page         | `/:lang/:slug/`            | `/zh/about/`             |
| Slide        | `/:lang/slides/:slug/`     | `/en/slides/my-talk/`    |
| Feed         | `/:lang/feed.xml`          | `/zh/feed.xml`           |
| Pagination   | `/:lang/:section/page/:n/` | `/en/posts/page/2/`      |

---

## Verification Protocol

### Before Claiming Done

1. **Run `nix build`** - must succeed with zero errors
2. **Test both languages** - check `/en/` and `/zh/` paths
3. **Check existing patterns** - `grep -r "pattern" src/` before adding new code

### Red Flags - Stop and Reconsider

- Adding MVar/IORef for caching
- Adding Haskell deps for tools Nix can provide
- Caching `_cache/` directory in CI
- "Clever" solutions to save build time
- Keeping unused code "for future use"

---

## Problem-Solving Approach

### When Stuck (3-Strike Rule)

After 3 failed attempts at the same fix → STOP → The problem is likely at a
different level:

1. **Implementation issue?** → Check if design is correct
2. **Design issue?** → Check if requirements are understood
3. **Requirements unclear?** → Ask for clarification

### Uncertainty Handling

| Confidence | Reversible? | Action                        |
| ---------- | ----------- | ----------------------------- |
| High       | Any         | Proceed                       |
| Medium     | Yes         | Proceed with noted assumption |
| Medium     | No          | Ask for confirmation          |
| Low        | Any         | Ask before proceeding         |

**Rule**: Never guess on irreversible actions (git push, file deletion, config
changes).
