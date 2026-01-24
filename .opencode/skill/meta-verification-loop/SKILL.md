---
name: meta-verification-loop
description: "META: Checkpoint and continuous verification. Never claim success without evidence."
type: meta
---

# Verification Loop

> Never claim success without evidence. Run commands, show output.

## Two Verification Modes

### Mode 1: Checkpoint Verification

Use between major steps to ensure incremental progress.

```bash
# Quick check - run after each significant change
nix build && echo "✅ Build passes"
```

### Mode 2: Continuous Verification

Full verification before claiming any task complete.

```bash
# Full verification suite
nix build                              # Build
nix develop --command cabal test       # Tests
nix develop --command hlint src/       # Linting
```

---

## Verification Checklist

| Check | Command | Pass Criteria |
|-------|---------|---------------|
| Build | `nix build` | Exit 0, no errors |
| Tests | `cabal test` | All examples pass |
| HLint | `hlint src/` | "No hints" |
| Format | `ormolu --mode check src/**/*.hs` | No diffs |

## Layer-Specific Checks

### L1 (Implementation)

```bash
# Type correctness
nix build 2>&1 | grep -E "error|warning" || echo "✅ No issues"

# Duplicate functions
grep -rh "^[a-z][a-zA-Z0-9_]* ::" src/ --include="*.hs" | \
  sed 's/ ::.*//' | sort | uniq -c | sort -rn | head -5

# Unsafe functions
grep -rn "fromJust\|head \|tail \|error " src/ --include="*.hs"
```

### L2 (Architecture)

```bash
# Module structure
find src/ -name "*.hs" -exec basename {} \; | sort

# Import consistency
grep -rh "^import" src/ --include="*.hs" | sort | uniq -c | sort -rn | head -10

# Circular dependencies (manual check)
# Each module should have clear responsibilities
```

### L3 (Content)

```bash
# Bilingual coverage
for lang in en zh; do
  echo "$lang posts:"
  find content/posts -name "index.$lang.md" | wc -l
done

# Missing translations
find content/posts -type d -mindepth 1 -maxdepth 1 | while read dir; do
  [ -f "$dir/index.en.md" ] || echo "Missing: $dir/index.en.md"
  [ -f "$dir/index.zh.md" ] || echo "Missing: $dir/index.zh.md"
done
```

---

## Checkpoint Protocol

### When to Checkpoint

| Trigger | Action |
|---------|--------|
| After design decision | `/checkpoint design` |
| After implementing feature | `/checkpoint implementation` |
| Before switching context | `/checkpoint context-switch` |
| After fixing bug | `/checkpoint bugfix` |

### Checkpoint Format

```markdown
## Checkpoint: [type]

**Time**: [timestamp]
**Layer**: L[X]
**Task**: [what was being done]

### State
- Build: ✅ / 🔴
- Tests: ✅ / 🔴
- Changes: [file list]

### Next Steps
1. [what to do next]
2. [what to do after that]

### Rollback Point
```bash
git stash  # or specific commit
```
```

---

## Verification Report Format

```markdown
## Verification Report

**Task**: [description]
**Time**: [timestamp]

### Checks

| Check | Result | Evidence |
|-------|--------|----------|
| Build | ✅ | `nix build` succeeded |
| Tests | ✅ | 14/14 pass |
| HLint | ✅ | No hints |
| Format | ✅ | No diffs |

### Layer Checks

| Layer | Check | Result |
|-------|-------|--------|
| L1 | No unsafe functions | ✅ |
| L2 | Module structure clean | ✅ |
| L3 | Bilingual coverage | ✅ |

### Evidence Log

```
$ nix build
[output]

$ cabal test
14 examples, 0 failures
```

## Verdict

✅ **VERIFIED** - All checks pass with evidence
🟡 **PARTIAL** - Some checks pass, issues noted
🔴 **FAILED** - Critical checks fail
```

---

## Anti-Patterns

| Don't | Instead |
|-------|---------|
| "Build should pass" | Run `nix build`, show output |
| "Tests look good" | Run `cabal test`, show results |
| "Code is clean" | Run `hlint`, show "No hints" |
| "Done" | Show verification report |

---

## Integration

**Triggered by**:
- `/verify` command
- `/checkpoint` command
- Validator agent
- Before any "completion" claim

**Outputs to**:
- Console (evidence)
- Checkpoint file (state)
- Validator (for final verdict)
