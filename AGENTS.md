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
├── packages/             # Local Nix packages (manually packaged dependencies)
│   ├── default.nix       # Auto-discovers packages in subdirectories
│   └── {pkg-name}/       # Each package in its own directory
│       └── default.nix   # Package derivation
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
- `packages/` - Local Nix packages for dependencies not in nixpkgs
- `config.yaml` - Site metadata, navigation, social links
- `hie.yaml` - HLS configuration (Cabal cradle)
- `package.yaml` - Haskell package definition (hpack)

## Local Nix Packages

External dependencies not available in nixpkgs are centrally managed in the `packages/` directory. This follows the [NUR packages](https://github.com/codgician/nur-packages) pattern:

- **Auto-discovery**: Packages are automatically discovered by `packages/default.nix`
- **One package = one directory**: Each package lives in `packages/{pkg-name}/default.nix`
- **Naming convention**: Use kebab-case for package names (e.g., `lucide-static`)
- **Self-contained**: Each package directory contains all necessary files (default.nix, patches, etc.)
- **Proper metadata**: Include `meta` with `description`, `homepage`, `license`, and `maintainers`

To add a new package:

1. Create `packages/{pkg-name}/default.nix`
1. Write the derivation using standard nixpkgs patterns
1. Import via `localPkgs.{pkg-name}` in `flake.nix`
1. No manual registration needed - auto-discovered

## Guidance for AI Agents

1. **Always run `nix build`** before claiming work is complete
1. **Check `nix develop`** works if modifying flake.nix
1. **Use existing patterns** - check similar code before adding new functionality
1. **Keep dependencies minimal** - avoid adding new Haskell packages unless necessary
1. **Prefer Text over String** - the codebase uses Data.Text throughout
1. **Test bilingual content** - ensure changes work for both en/zh languages

## Multilingual Architecture Principles

1. **No hardcoded language codes**: All language iteration must use `languages` from config.yaml. Never hardcode `["en", "zh"]` in Haskell code.
2. **Config fallback**: Translated strings in config.yaml should fall back to the default language (first in `languages` list) if a translation is not provided. This allows defining only `en:` when values are the same across languages.
3. **All pages in all languages**: Every content page (posts, standalone pages) should generate HTML for ALL configured languages, using fallback content when a translation doesn't exist.
4. **Language-consistent URLs**: Within a language context, all internal links should stay in that language. For example, the English blog list should link to `/en/posts/{slug}/` even if the post content falls back to Chinese.
5. **Nested content support**: Standalone pages can be nested (e.g., `content/icpc/templates/math/index.zh.md` → `/zh/icpc/templates/math/`).
