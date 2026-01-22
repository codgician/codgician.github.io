---
name: coding-standard
description: "CORE: L1 implementation standards for Haskell/SCSS/Nix. How to write correct code."
type: core
layer: L1
---

# Coding Standard (L1: Implementation)

## Core Question

**Is this code correct by construction?**

## Quick Reference

| Aspect | Standard |
|--------|----------|
| Types | Explicit signatures on all functions |
| Text | Use `Text` not `String` (except Hakyll APIs) |
| Imports | Explicit: `import Data.Maybe (fromMaybe)` |
| Errors | `Maybe`/`Either`, never `error` |
| External tools | Subprocess via `readProcess` |
| Dependencies | Nix only (`flake.nix`) |

## Thinking Prompt

Before implementing:
1. Is this at a Hakyll API boundary? (They use `String`)
2. Does similar code exist? (`grep -r "pattern" src/`)
3. Can this function fail? → Use `Maybe`/`Either`

## Trace UP ↑ (When to Escalate)

| Symptom | Indicates |
|---------|-----------|
| Same error after 3 fixes | Wrong layer - design issue |
| "Where should this live?" | Architecture question (L2) |
| Type gymnastics | Design smell |

## Anti-Patterns

| Don't | Better |
|-------|--------|
| `fromJust` | Pattern match or `fromMaybe` |
| `error "msg"` | Return `Maybe`/`Either` |
| Wildcard imports | Explicit imports |
| Copy-paste code | Extract helper |

## Validation Commands

```bash
nix build  # Must pass

# Check duplicates
grep -rh "^[a-z][a-zA-Z0-9_]* ::" src/ --include="*.hs" | \
  sed 's/ ::.*//' | sort | uniq -c | sort -rn | head -5
```
