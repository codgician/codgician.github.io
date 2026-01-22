---
name: code-review
description: Review code changes against project coding standards. Use when completing tasks, reviewing PRs, or before merging. Provides evidence-based assessment with specific file:line references.
---

# Code Review

Evidence-based review against project coding standards.

**Core principle:** Search, don't read. Run commands, show evidence.

## When to Use

- After implementing changes
- Before merging to main
- When reviewing PRs
- Periodic codebase health checks

## Review Process

### 1. Gather Context

```bash
# What changed?
git diff --stat HEAD~1..HEAD
git diff HEAD~1..HEAD -- "*.hs" "*.scss"
```

### 2. Run Automated Checks

#### Build Verification
```bash
nix build
```

#### Haskell Quality
```bash
# Duplicate function names (count > 1 is suspicious)
grep -rh "^[a-z][a-zA-Z0-9_]* ::" src/ --include="*.hs" | \
  sed 's/ ::.*//' | sort | uniq -c | sort -rn | head -10

# Missing type signatures
grep -E "^[a-z][a-zA-Z0-9_]+ [^:]" src/*.hs | grep -v "where" | head -5

# String instead of Text (except Hakyll APIs)
grep -n ":: String" src/*.hs | grep -v "FilePath\|Identifier" | head -5
```

#### SCSS Quality
```bash
# Hardcoded pixels (excluding media queries)
grep -nE "[0-9]+px" static/scss/*.scss | grep -v "//" | grep -v "@media" | head -10

# Hardcoded colors
grep -nE "#[0-9a-fA-F]{3,6}" static/scss/*.scss | grep -v "//" | head -10
```

### 3. Manual Checks (Against Coding Standard)

Reference: `.opencode/skills/coding-standard/`

| Check | How |
|-------|-----|
| Type signatures | All top-level functions have signatures |
| Text vs String | `Text` used except Hakyll APIs |
| Explicit imports | No wildcard imports |
| Context composition | Order: specific → shared → default |
| Helper extraction | Patterns repeated 3+ times extracted |
| SCSS variables | No hardcoded spacing/colors |

### 4. Report Format

```markdown
## Code Review: [Feature/Change]

### Summary
| Check | Result |
|-------|--------|
| Build | ✅ Pass / 🔴 Fail |
| Duplicates | ✅ None / 🔴 Found X |
| Type signatures | ✅ Present / 🟡 Missing X |
| SCSS hardcodes | ✅ None / 🟡 Found X |

### Issues

#### 🔴 Critical (must fix)
[Build failures, duplicates, broken functionality]

#### 🟡 Warning (should fix)
[Coding standard violations, hardcoded values]

#### 💡 Suggestions
[Improvements, not blocking]

### Evidence
[Paste command outputs]

### Verdict
✅ **APPROVED** - Ready to merge
🟡 **CONDITIONAL** - Fix warnings first
🔴 **BLOCKED** - Critical issues found
```

## Issue Severity

| Severity | Criteria |
|----------|----------|
| 🔴 Critical | Build fails, duplicate definitions, broken tests |
| 🟡 Warning | Coding standard violations, hardcoded values |
| 💡 Suggestion | Improvements, minor style issues |

## Common Issues

| Issue | Fix |
|-------|-----|
| Duplicate function name | Rename or extract to shared module |
| Missing type signature | Add explicit signature |
| `String` instead of `Text` | Change to `Text`, add conversion at boundary |
| Hardcoded pixel | Use `$space-*` variable |
| Hardcoded color | Use `--color-*` CSS variable |
| Copy-pasted pattern | Extract to helper or placeholder |

## Quick Commands

```bash
# Full review script
nix build && \
  echo "=== Duplicates ===" && \
  grep -rh "^[a-z][a-zA-Z0-9_]* ::" src/ --include="*.hs" | sed 's/ ::.*//' | sort | uniq -c | sort -rn | head -5 && \
  echo "=== SCSS Hardcodes ===" && \
  grep -nE "[0-9]+px|#[0-9a-fA-F]{3,6}" static/scss/*.scss | grep -v "//" | grep -v "@media" | head -10
```
