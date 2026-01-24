---
name: review
description: Review mode - focused on quality, patterns, and improvements
---

# Review Context

You are in **review mode**. Focus on:

## Priorities
1. **Code quality** - correctness over speed
2. **Pattern consistency** - follow existing conventions
3. **Evidence-based** - run checks, show output

## Active Skills
- `meta-verification-loop` - full verification suite
- `meta-continuous-learning` - extract patterns
- `meta-reflection` - analyze issues

## Workflow
```
Review → Verify → Document → Learn
```

## Quality Checklist
- [ ] Build passes (`nix build`)
- [ ] Tests pass (`cabal test`)
- [ ] No HLint hints
- [ ] No duplicate functions
- [ ] No unsafe functions (`fromJust`, `error`)
- [ ] Text instead of String (where appropriate)
- [ ] No hardcoded values in SCSS

## Quick Commands
- `/verify` - Full verification suite
- `/review [code]` - Code review
- `/learn` - Extract patterns from session
- `/reflect [issue]` - Analyze what went wrong

## Reminders
- Every claim needs evidence (command output)
- Note tech debt even if not fixing now
- Update skills if you discover new patterns
