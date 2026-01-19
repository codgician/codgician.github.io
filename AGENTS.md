# AI Agent Guidelines

This document outlines the core principles and conventions for agents working on this repository.

## Project Summary

A minimalist personal website and blog built with **Hakyll** and **Nix**. The site prioritizes correctness, simplicity, and a clean filesystem hierarchy over complex optimizations.

## Core Coding Principles

1. **Pragmatism over Purity**: Favor external tools (e.g., KaTeX, Mermaid) via subprocesses rather than complex Haskell integrations.
1. **Simplicity over Cleverness**: Use Hakyll's built-in template system instead of more type-safe but complex alternatives like Lucid or Heist.
1. **Correctness over Performance**: Prefer fresh builds (~20-30s) over fragile caching mechanisms that risk correctness issues.
1. **Content-Addressed Caching**: Cache expensive operations (KaTeX, Mermaid) using content hashes that include tool versions.
1. **Config-Driven Architecture**: Manage navigation, social links, and site metadata through centralized YAML configuration.
1. **Bilingual First**: Maintain i18n support (English/Chinese) with `canonical` flags for AI-assisted translation workflows.
1. **Nix is Truth**: All build dependencies and tools must be declared in `flake.nix` for reproducibility.
1. **Consistent Iconography**: Use Lucide icons (via `lucide-static` from npm, bundled at build time) for all UI elements. Avoid mixing emoji with icons.

## Build System

- **Primary build**: `nix build` - produces the complete website in `result/`
- **Development**: `nix develop` - enters shell with HLS, cabal, ormolu, hlint
- **Watch mode**: `nix run . -- watch` - auto-rebuild on changes (CSS won't compile in watch mode)
- **Formatting**: `nix fmt` - formats Haskell, Nix, Markdown, YAML

## Project Structure

```
.
├── app/Main.hs           # CLI entry point
├── src/
│   ├── Site.hs           # Main Hakyll rules
│   ├── Config.hs         # YAML config loader
│   ├── Context.hs        # Template context fields
│   ├── Feed.hs           # RSS feed generation
│   └── Compiler/
│       ├── Pandoc.hs     # Markdown processing
│       ├── KaTeX.hs      # Math rendering
│       ├── Mermaid.hs    # Diagram rendering
│       └── Cache.hs      # Content-addressed cache
├── content/
│   ├── posts/{slug}/     # Blog posts (index.en.md, index.zh.md)
│   ├── index.en.md       # Homepage (English)
│   └── index.zh.md       # Homepage (Chinese)
├── templates/            # Hakyll HTML templates
├── static/               # SCSS, JS, images
├── flake.nix             # Nix flake (build system)
└── config.yaml           # Site configuration
```

## Key Files

- `flake.nix` - Nix build configuration, devShell, and dependencies
- `config.yaml` - Site metadata, navigation, social links
- `hie.yaml` - HLS configuration (Cabal cradle)
- `package.yaml` - Haskell package definition (hpack)

## Guidance for AI Agents

1. **Always run `nix build`** before claiming work is complete
1. **Check `nix develop`** works if modifying flake.nix
1. **Use existing patterns** - check similar code before adding new functionality
1. **Keep dependencies minimal** - avoid adding new Haskell packages unless necessary
1. **Prefer Text over String** - the codebase uses Data.Text throughout
1. **Test bilingual content** - ensure changes work for both en/zh languages
