# codgician.github.io

Personal homepage and blog built with [Hakyll](https://jaspervdj.be/hakyll/) and [Nix](https://nixos.org/).

## Features

- Bilingual support (English/Chinese)
- Server-side KaTeX math rendering
- Server-side Mermaid diagram rendering
- Dark/light theme with system preference detection
- RSS feeds per language
- Fully reproducible builds via Nix

## Quick Start

### Prerequisites

- [Nix](https://nixos.org/download.html) with flakes enabled

### Build the Site

```bash
# Build the complete website
nix build

# Output is in ./result/
ls result/
```

### Local Development

```bash
# Run the site generator with watch mode (auto-rebuild on changes)
nix run . -- watch

# The site will be available at http://127.0.0.1:8000
```

### Development Shell

```bash
# Enter a development shell with all tools (HLS, cabal, ormolu, etc.)
nix develop

# Then you can use cabal for incremental builds during development
cabal build
```

### Other Commands

```bash
# Clean build artifacts
nix run . -- clean

# Check site for broken links
nix run . -- check

# Rebuild from scratch
nix run . -- rebuild
```

## Project Structure

```
.
├── content/           # Markdown content
│   ├── posts/         # Blog posts (folder per post)
│   │   └── {slug}/
│   │       ├── index.en.md
│   │       └── index.zh.md
│   ├── index.en.md    # English homepage
│   └── index.zh.md    # Chinese homepage
├── templates/         # Hakyll templates
├── static/            # Static assets
│   ├── scss/          # Sass stylesheets
│   └── js/            # JavaScript
├── src/               # Haskell source
├── config.yaml        # Site configuration
└── flake.nix          # Nix flake
```

## Writing Posts

Create a new post by adding a folder under `content/posts/`:

```bash
mkdir -p content/posts/my-new-post
```

Then create `index.en.md` (and optionally `index.zh.md`):

```markdown
---
title: "My New Post"
date: 2025-01-19
language: en
canonical: true
math: false
mermaid: false
---

Your content here...
```

### Frontmatter Options

| Field | Description |
|-------|-------------|
| `title` | Post title |
| `date` | Publication date (YYYY-MM-DD) |
| `language` | Content language (`en` or `zh`) |
| `canonical` | `true` if this is the source language |
| `math` | `true` to enable KaTeX rendering |
| `mermaid` | `true` to enable Mermaid diagrams |

# License

[CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/)