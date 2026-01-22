---
name: tdd-workflow
description: "CORE: Test-driven development workflow. Define verification BEFORE implementing."
type: core
layer: L1
---

# TDD Workflow

> Define verification BEFORE implementing.

## Core Question

**How will we know this is correct?**

## The Cycle

```
1. DEFINE  → Write verification command
2. FAIL    → Run it, confirm failure
3. IMPLEMENT → Write code
4. PASS    → Verify success
5. REFACTOR → Clean up, verify still passes
```

## Verification Patterns

| Feature Type | Verification |
|--------------|-------------|
| New route | `test -f result/path/file.html` |
| Template field | `grep -q "value" result/page.html` |
| RSS | `grep -c "<entry>" result/lang/feed.xml` |
| Bilingual | `test -f result/en/... && test -f result/zh/...` |

## Example

```bash
# 1. Define (should fail - feature doesn't exist)
nix build && test -f result/en/new-feature/index.html

# 2. Implement the feature

# 3. Verify (should pass now)
nix build && test -f result/en/new-feature/index.html
```

## Quick Checklist

- [ ] Verification written BEFORE implementing
- [ ] Verification FAILS initially
- [ ] `nix build` passes after implementation
- [ ] Verification PASSES after implementation
