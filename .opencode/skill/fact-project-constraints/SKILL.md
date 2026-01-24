---
name: fact-project-constraints
description: "FACT: Non-negotiable project rules - bilingual, minimalist, Nix-only, build must pass."
type: fact
---

# Project Constraints

> These are **NON-NEGOTIABLE**. All decisions must satisfy these constraints.

## Core Constraints

| Constraint | Implication | Violation Example |
|------------|-------------|-------------------|
| **Bilingual (en/zh)** | Every content page needs both language versions | Page only in English |
| **Minimalist** | Simple > clever, no unnecessary features | Adding unused dependencies |
| **Nix-managed** | All dependencies via `flake.nix` | Manual `npm install` |
| **Build must pass** | `nix build` is the single source of truth | "It works on my machine" |

## Bilingual Requirements

```
Content requires:
├── /en/... version
├── /zh/... version
├── Language switcher on every page
└── Fallback behavior when translation missing
```

**Fallback rule**: If translation doesn't exist, link to the version that does exist (don't show broken link).

## Minimalist Philosophy

**Add features only when**:
1. There's a clear user need
2. Simpler alternatives were considered
3. Maintenance cost is acceptable

**Remove before adding**: If something isn't used, remove it rather than work around it.

## Nix as Truth

```bash
# The ONLY way to build
nix build

# The ONLY way to get a dev environment
nix develop

# Dependencies ONLY via
flake.nix → inputs/packages
```

**Never**:
- Install tools globally
- Use system packages directly
- Bypass Nix for "convenience"

## Build Verification

Every change must pass:
```bash
nix build  # Must succeed with zero errors
```

Optional but recommended:
```bash
nix develop --command cabal test  # If tests exist
nix develop --command hlint src/  # Should show "No hints"
```

## URL Structure

| Content Type | Pattern | Example |
|--------------|---------|---------|
| Post | `/:lang/posts/:slug/` | `/en/posts/hello-world/` |
| Page | `/:lang/:slug/` | `/zh/about/` |
| Slide | `/:lang/slides/:slug/` | `/en/slides/my-talk/` |
| Feed | `/:lang/feed.xml` | `/zh/feed.xml` |
| Pagination | `/:lang/:section/page/:n/` | `/en/posts/page/2/` |

## File Organization

```
content/
├── posts/
│   └── {slug}/
│       ├── index.en.md
│       ├── index.zh.md
│       └── assets/        # Co-located images
├── pages/
│   └── {slug}.{lang}.md
└── slides/
    └── {slug}/
        └── slides.md      # Single language, version in route
```

## Technical Stack

| Layer | Technology | Locked? |
|-------|------------|---------|
| Generator | Hakyll | ✅ Yes |
| Language | Haskell | ✅ Yes |
| Build | Nix Flakes | ✅ Yes |
| Styling | SCSS | ✅ Yes |
| Math | KaTeX (server-side) | ✅ Yes |
| Diagrams | Mermaid (server-side) | ✅ Yes |
