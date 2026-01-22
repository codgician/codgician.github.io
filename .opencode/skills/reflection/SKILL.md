---
name: reflection
description: "Analyze failures and improve the system. TRIGGERS: agent failed, wrong advice, repeated mistake. Use in current session (needs history)."
---

# Reflection

> Analyze what went wrong and fix the system, not just this instance.

**Important**: This is a skill, not an agent - it needs conversation history.

## 5-Question Reboot

| # | Question |
|---|----------|
| 1 | What were we trying to achieve? |
| 2 | What actually happened? |
| 3 | What layer was the problem in? (L1/L2/L3) |
| 4 | What was assumed vs. verified? |
| 5 | How do we prevent this systemically? |

## Root Cause Categories

| Category | Fix Type |
|----------|----------|
| Wrong layer | Add trace direction guidance |
| Missing check | Add to verification steps |
| Wrong assumption | Add "verify before assuming" rule |
| Missing context | Add to agent's knowledge |

## Output Format

```markdown
## Reflection: [Issue]

### 5-Question Analysis
1. Goal: [what we wanted]
2. Happened: [the failure]
3. Layer: L[X]
4. Gap: [assumed X, should have verified Y]
5. Fix: [systemic change]

### Proposed Change
**File**: `.opencode/[path]`
**Change**: [specific edit]
**Prevents**: [how this stops recurrence]
```
