# AI Agent Guidelines

Essential context for AI coding assistants working on this repository.

## Project Overview

A minimalist personal website and blog built with **Hakyll** (Haskell static site generator) and **Nix** (reproducible builds). Supports bilingual content (English/Chinese).

**Tech Stack**: Haskell, Hakyll, Nix, SCSS, Pandoc, KaTeX, Mermaid

## Quick Commands

```bash
nix build              # Build site (always run before claiming done)
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

## Core Principles

1. **Simplicity over Cleverness** - Prefer boring solutions
2. **Correctness over Performance** - Fresh builds > fragile caching
3. **Nix is Truth** - All dependencies in `flake.nix`
4. **External Tools via Subprocess** - KaTeX/Mermaid via CLI

## Critical Conventions

| Area | Convention |
|------|------------|
| **Text** | Use `Data.Text`, not `String` |
| **Languages** | Never hardcode `["en", "zh"]` - use config |
| **Posts** | One folder per post: `content/posts/{slug}/index.{lang}.md` |
| **Styling** | SCSS variables only - no hardcoded px or colors |
| **Icons** | Use `icon-*` class prefix (Lucide font) |

## Agent System

This repo uses OpenCode agents for structured development.

### Commands (User Entry Points)

| Command | Purpose |
|---------|---------|
| `/feature <desc>` | New feature → Planner orchestrates |
| `/fix <issue>` | Bug fix → Coder directly |
| `/design <desc>` | UI work → Designer |
| `/architect <desc>` | Technical design → Tech Lead |
| `/review` | Quality check → Validator |
| `/reflect <incident>` | Process improvement → Reflector |

### Agents (`.opencode/agents/`)

| Agent | Mode | Role |
|-------|------|------|
| `planner` | primary | Orchestrates team via Task tool |
| `tech-lead` | subagent | Research + Architecture |
| `designer` | subagent | Visual/UX design |
| `coder` | subagent | Implementation |
| `validator` | subagent | Quality gate |
| `reflector` | subagent | Process improvement |

### Skills (`.opencode/skills/`)

- `hakyll-minimalist` - Coding patterns for this project
- `hakyll-design-system` - Design tokens and component specs

## Before Completing Any Task

1. Run `nix build` - must succeed
2. Test both languages (`/en/` and `/zh/`)
3. Check existing patterns before adding new code
