---
name: hakyll-architecture
description: "CORE: L2 architecture patterns for Hakyll - routes, templates, compilers, contexts."
type: core
layer: L2
---

# Hakyll Architecture (L2: Structure)

## Core Question

**How should this be structured to be simple and maintainable?**

## Quick Reference

| Pattern | Example |
|---------|---------|
| Routes | `/:lang/posts/:slug/` |
| Context composition | `specific <> shared <> defaultContext` |
| Templates | Compose via `$partial()$` |
| Caching | Content-addressed in `_artifacts/` |

## Thinking Prompt

Before designing:
1. What content types are involved?
2. Can we reuse existing patterns? (`grep -r "match" src/Site.hs`)
3. What's the simplest approach?

## Trace UP ↑ (Check L3 Constraints)

| Question | Check |
|----------|-------|
| What URL structure? | Content organization (L3) |
| What goes in feed? | Reader needs (L3) |
| How handle translations? | Bilingual strategy (L3) |

## Trace DOWN ↓ (Implementation)

| Decision | Implementation |
|----------|----------------|
| Language in URL | `extractLang` helper |
| RSS per language | Filter posts by `lang` field |
| Server-side render | Subprocess for KaTeX/Mermaid |

## Project Decisions

| Decision | Rationale |
|----------|-----------|
| Language in URL (`/en/`, `/zh/`) | SEO, bookmarkable |
| Post folders (`posts/slug/`) | Co-locate assets |
| Nix for external tools | Reproducible builds |
