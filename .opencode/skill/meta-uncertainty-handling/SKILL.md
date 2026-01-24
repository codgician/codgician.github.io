---
name: meta-uncertainty-handling
description: "META: Decide when to ask, proceed, or defer based on confidence and reversibility."
type: meta
---

# Uncertainty Handling

> When unsure, use this framework to decide: **Ask, Proceed, or Defer**.

## The Decision Matrix

| Confidence | Reversible? | Risk | Action |
|------------|-------------|------|--------|
| High | Any | Any | **Proceed** |
| Medium | Yes | Low | **Proceed** with assumption noted |
| Medium | Yes | High | **Checkpoint** then proceed |
| Medium | No | Any | **Ask** for confirmation |
| Low | Yes | Low | **Try** with checkpoint |
| Low | Yes | High | **Ask** or defer to another agent |
| Low | No | Any | **Ask** - never guess on irreversible |

## Reversibility Guide

| Action | Reversible? | Notes |
|--------|-------------|-------|
| Read files | ✅ Yes | No side effects |
| Edit files | ✅ Yes | Git can restore |
| Run `nix build` | ✅ Yes | No side effects |
| Delete files | ⚠️ Partially | Git can restore if committed |
| `git commit` | ⚠️ Partially | Can amend/reset if not pushed |
| `git push` | ❌ No | Public history changed |
| Modify config | ⚠️ Depends | Check if backed up |

## Risk Assessment

| Indicator | Risk Level |
|-----------|------------|
| Touches multiple files | Higher |
| Changes public API | Higher |
| Modifies build config | Higher |
| Single file change | Lower |
| Additive change (new function) | Lower |
| Has existing tests | Lower |

## Layer-Specific Defaults

| Layer | Default Stance | Rationale |
|-------|----------------|-----------|
| L3 (Content/Feature) | **Ask** | User intent matters most |
| L2 (Architecture) | **Ask** or research | Design decisions are consequential |
| L1 (Implementation) | **Proceed** if reversible | Code is cheap to try |

## Output Format

When uncertain, state:
```markdown
**Uncertainty**: [what I'm unsure about]
**Confidence**: LOW / MEDIUM
**Reversibility**: YES / NO / PARTIAL
**Risk**: LOW / MEDIUM / HIGH

**Decision**: ASK / PROCEED / DEFER

**If proceeding**: [assumption I'm making]
**If asking**: [specific question]
**If deferring**: [to whom/what]
```

## Examples

### Example 1: Uncertain about implementation detail
```
Task: Add pagination to slides
Uncertainty: Should page size match posts (10) or be different?
Confidence: MEDIUM (could check config)
Reversibility: YES (just a number)
Risk: LOW

Decision: PROCEED with assumption
Assumption: Use same page size as posts for consistency
```

### Example 2: Uncertain about feature scope
```
Task: "Make the site faster"
Uncertainty: What specifically? Images? Build time? Runtime?
Confidence: LOW (vague request)
Reversibility: N/A (haven't started)
Risk: HIGH (could waste effort)

Decision: ASK
Question: "What's slow? Page load time, build time, or something else?"
```

### Example 3: Uncertain about design
```
Task: Add RSS feed for slides
Uncertainty: Should slides even have RSS? They're presentations, not articles.
Confidence: LOW (L3 question)
Reversibility: YES (can remove later)
Risk: MEDIUM (might not match user expectations)

Decision: ASK
Question: "Do you want RSS for slides? They're typically viewed as presentations rather than subscribed to."
```
