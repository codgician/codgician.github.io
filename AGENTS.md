# AI Agent Guidelines

This document provides essential context for AI coding assistants working on this repository.

## Project Overview

A minimalist personal website and blog built with **Hakyll** (Haskell static site generator) and **Nix** (reproducible builds). Supports bilingual content (English/Chinese).

**Tech Stack**: Haskell, Hakyll, Nix, SCSS, Pandoc, KaTeX, Mermaid

## Quick Commands

```bash
# Build the site (primary command - always run before claiming work is done)
nix build

# Development shell with HLS, cabal, ormolu, hlint
nix develop

# Watch mode - auto-rebuild on changes
nix run . -- watch

# Format all code (Haskell, Nix, Markdown, YAML)
nix fmt
```

## Repository Structure

```
.
├── src/                    # Haskell source
│   ├── Site.hs             # Main Hakyll rules
│   ├── Config.hs           # YAML config loader
│   ├── Context.hs          # Template context fields
│   └── Compiler/           # Pandoc, KaTeX, Mermaid, Cache
├── content/                # Markdown content
│   ├── posts/{slug}/       # Blog posts (index.en.md, index.zh.md)
│   └── index.{lang}.md     # Homepage per language
├── templates/              # Hakyll HTML templates
├── static/scss/            # SCSS stylesheets
├── config.yaml             # Site configuration
└── flake.nix               # Nix build system
```

## Core Principles

1. **Simplicity over Cleverness** - Prefer boring solutions
2. **Correctness over Performance** - Fresh 20-30s builds > fragile caching
3. **Nix is Truth** - All dependencies in `flake.nix`
4. **External Tools via Subprocess** - KaTeX/Mermaid via CLI, not Haskell bindings

## Critical Conventions

| Area | Convention |
|------|------------|
| **Text** | Use `Data.Text`, not `String` |
| **Languages** | Never hardcode `["en", "zh"]` - use `languages` from config.yaml |
| **Posts** | One folder per post: `content/posts/{slug}/index.{lang}.md` |
| **Styling** | SCSS variables only - no hardcoded px or colors |
| **Dependencies** | Add to both `package.yaml` AND `flake.nix` |

## Before Completing Any Task

1. Run `nix build` - must succeed
2. Test both languages work (`/en/` and `/zh/`)
3. Check existing patterns before adding new code

## Additional Resources

- **Detailed coding patterns**: Load the `hakyll-minimalist` skill
- **Design system tokens**: Load the `hakyll-design-system` skill
- **Agent-specific instructions**: See `.opencode/agents/` for specialized agents
