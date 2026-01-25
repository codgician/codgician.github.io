# Architecture Principles

Project philosophy and guardrails. Read this before adding features or dependencies.

## Core Principles

1. **Pragmatism over Purity** - External tools via subprocess > complex Haskell integrations
2. **Simplicity over Cleverness** - If it feels clever, it's probably wrong
3. **Correctness over Performance** - Fresh builds > fragile caching
4. **Content-Addressed Caching** - Hash includes: content + tool version + options + filter version
5. **Nix is Truth** - All dependencies declared in `flake.nix`

## Decision Table

| Situation | Do This | Not This |
|-----------|---------|----------|
| Add external tool | Subprocess via `readProcess` | FFI or Haskell binding |
| Need caching | Content-addressed in `_artifacts/` | Persist `_cache/` in CI |
| Template logic | Hakyll built-in `$if$`/`$for$` | Lucid/Heist |
| CI build slow | Accept 20-30s | Complex cache invalidation |
| New dependency | Add to `package.yaml` + `flake.nix` | `cabal install` or `stack install` |
| Multi-language content | `content/posts/{slug}/index.en.md` | Language subdirectories |
| Styling | SCSS variables in `static/scss/` | Inline styles or CSS frameworks |
| Unused feature | Delete it | Comment out "for later" |
| Code formatting | Run `treefmt` via Nix | Manual formatting |
| Site metadata | Use `config.yaml` | Hardcode in templates |

## Caching Strategy

```
_cache/     → Local only, NEVER cache in CI (mtime unreliable)
_artifacts/ → Content-addressed, safe to cache in CI
             Key = SHA256(formatVersion + toolName + toolVersion + toolOptions + filterVersion + content)
```

## Red Flags - STOP and Reconsider

Before implementing, check if you're doing any of these:

### Haskell/Build
- Adding MVar/IORef for "thread-safe" caching
- Implementing automatic garbage collection
- Adding Haskell dependencies for tools Nix can provide
- "Clever" solutions to save 15 seconds of build time
- Running `cabal install` or `stack install` outside Nix
- Adding Haskell dep to `package.yaml` without system lib in `flake.nix`

### Caching
- Caching `_cache/` directory in CI
- Using `git-restore-mtime` for cache correctness
- Tool version not in cache key

### Content/Templates
- Creating language subdirectories instead of file suffixes
- Hardcoding site metadata (author, title, social links) in templates
- Keeping unused code "for future use" (YAGNI)

### Frontend
- Adding CSS frameworks (Tailwind, Bootstrap) or CSS-in-JS
- Adding `<script>` tags for CDN libraries (use server-side rendering)
- Git submodules for external dependencies (use Nix)

**All of these violate project principles. Simpler is better.**

## Self-Check Questions

Before implementing anything, ask:

1. Does this add a new Haskell dependency? → Can Nix provide it instead?
2. Does this cache something? → Is content-addressing sufficient?
3. Does this save build time? → Is 20-30s rebuild acceptable?
4. Does this feel clever? → Is there a boring alternative?

## Content Structure

```
content/
├── posts/{slug}/
│   ├── index.en.md      # English version
│   ├── index.zh.md      # Chinese version
│   └── *.png/jpg        # Post-specific images
├── slides/{slug}/
│   ├── slides.md        # Reveal.js slides
│   └── *.png/svg        # Slide images (flat, no subdirs)
└── index.{lang}.md      # Homepage per language
```

### Post Frontmatter

```yaml
---
title: "Post Title"
date: 2025-01-22
language: en
canonical: true          # true for source language
math: false              # enable KaTeX
mermaid: false           # enable Mermaid diagrams
---
```
