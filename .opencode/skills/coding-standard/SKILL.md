---
name: coding-standard
description: Project coding standards and architecture principles for Haskell/Hakyll development. Use when writing new code, reviewing changes, adding features, or making architectural decisions.
---

# Coding Standard

Shared conventions for this Hakyll blog project. Keep code simple, correct, and maintainable.

## Quick Reference

| Aspect | Standard |
|--------|----------|
| Types | Explicit signatures on all top-level functions |
| Text | Use `Text` not `String` (except Hakyll APIs) |
| Imports | Explicit, qualified where ambiguous |
| Error handling | Return `Maybe`/`Either`, avoid `error` |
| External tools | Subprocess via `readProcess`, never FFI |
| Caching | Content-addressed in `_artifacts/` |
| Dependencies | Nix only (`flake.nix` + `package.yaml`) |

## Detailed References

| Reference | Use When |
|-----------|----------|
| [references/haskell.md](references/haskell.md) | Writing Haskell code |
| [references/scss.md](references/scss.md) | Styling, design tokens |
| [references/architecture.md](references/architecture.md) | Adding features, dependencies, caching |

## Architecture Principles

See [references/architecture.md](references/architecture.md) for full details.

**Core rules:**
1. Pragmatism over Purity - Subprocess > complex integrations
2. Simplicity over Cleverness - If clever, probably wrong
3. Correctness over Performance - 20-30s rebuild is acceptable
4. Nix is Truth - All deps in `flake.nix`

## Haskell Conventions

See [references/haskell.md](references/haskell.md) for patterns.

**Key rules:**
- Type signatures required
- Compose contexts: `specific <> shared <> defaultContext`
- Extract helpers for repeated patterns (3+ uses)
- Use subprocess for external tools

## SCSS Conventions

See [references/scss.md](references/scss.md) for design tokens.

**Key rules:**
- Use spacing variables (`$space-*`)
- Use color CSS variables (`--color-*`)
- No hardcoded pixels or colors
- Extract placeholders for reused patterns

## Validation

Before completing any task:

```bash
# Build must pass
nix build

# Check for duplicates
grep -rh "^[a-z][a-zA-Z0-9_]* ::" src/ --include="*.hs" | \
  sed 's/ ::.*//' | sort | uniq -c | sort -rn | head -5

# Check for hardcoded values in SCSS
grep -E "[0-9]+px" static/scss/*.scss | grep -v "//" | head -5
```

## When in Doubt

1. Check [references/architecture.md](references/architecture.md) for red flags
2. Follow existing patterns in the codebase
3. Simpler is better
4. Ask before inventing new conventions
