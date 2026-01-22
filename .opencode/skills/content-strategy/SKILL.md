---
name: content-strategy
description: "L3 Content constraints and reader needs. TRIGGERS: feature, bilingual, i18n, RSS, reader, UX."
---

# Content Strategy (L3: Why)

## Core Question

**What does the reader need?**

## Constraints

| Constraint | Implication |
|------------|-------------|
| Bilingual (en/zh) | Paired pages, language switcher, separate RSS |
| Technical content | Math (KaTeX), diagrams (Mermaid) |
| Minimalist | No unnecessary features, fast loading |

## Thinking Prompt

Before planning features:
1. Who is the reader? (Technical, bilingual)
2. Does this align with minimalist philosophy?
3. Does it work in both languages?

## Trace DOWN ↓ (Feature → Implementation)

| Content Decision | Architecture (L2) | Implementation (L1) |
|------------------|-------------------|---------------------|
| Bilingual posts | Route: `/:lang/posts/:slug/` | `extractLang` function |
| Math support | Compiler: KaTeX transform | `cachedKaTeX` subprocess |
| RSS per language | Route: `/:lang/feed.xml` | Filter by `lang` field |

## Content Types

| Type | URL Pattern | Features |
|------|-------------|----------|
| Post | `/:lang/posts/:slug/` | Date, tags, math, diagrams |
| Page | `/:lang/:slug/` | Simple content |
| Feed | `/:lang/feed.xml` | Latest posts |

## Anti-Patterns

| Don't | Why |
|-------|-----|
| Client-side rendering | JS dependency, slow |
| Skip translation fallback | Broken links |
| Hardcoded language strings | Not bilingual-ready |
