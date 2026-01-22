---
name: hakyll-router
description: "Route Hakyll questions to the right layer. TRIGGERS: build error, template, route, compiler, context, bilingual, feature, refactor."
---

# Hakyll Router

> Identify layer, then load appropriate skill.

## Layer Detection

| Signal | Layer | Load Skill |
|--------|-------|------------|
| Build/type error, GHC error | L1 | `coding-standard` |
| SCSS error, styling | L1 | `coding-standard` |
| "How to design", structure | L2 | `hakyll-architecture` |
| Template, route, compiler | L2 | `hakyll-architecture` |
| "Add feature", bilingual | L3 | `content-strategy` |
| Reader needs, UX | L3 | `content-strategy` |

## Trace Directions

| Entry | Direction | Example |
|-------|-----------|---------|
| L1 error | UP ↑ | Build fails → Is this a design issue? |
| L2 design | Both | Check L3 constraints, then implement |
| L3 feature | DOWN ↓ | Feature → Structure → Implementation |

## Cross-Layer Problems

When L3 keywords appear with L1 errors, load both skills:
- Bilingual + error → `coding-standard` + `content-strategy`
- RSS + template → `hakyll-architecture` + `content-strategy`
