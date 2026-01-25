---
name: meta-continuous-learning
description: "META: Extract patterns from sessions and update skills. Learn from experience."
type: meta
---

# Continuous Learning

> Learn from each session. Update the system to prevent future issues.

## Purpose

This skill extracts reusable patterns from the current session and:
1. Persists them to memory
2. Suggests skill/agent updates
3. Prevents recurring mistakes

---

## Pattern Extraction Process

### Step 1: Identify Learnings

Scan the session for:

| Type | Indicators |
|------|------------|
| **Mistake** | "oops", "wrong", backtracking, 3+ attempts |
| **Discovery** | "actually", "turns out", "I found" |
| **Pattern** | Repeated solution across different problems |
| **Constraint** | Project-specific rule discovered |

### Step 2: Categorize

| Category | Destination | Example |
|----------|-------------|---------|
| Coding pattern | `skill-coding-standard` | "Always check for empty list" |
| Architecture decision | `fact-hakyll-architecture` | "Routes follow /:lang/:section/" |
| Content rule | `skill-content-strategy` | "Math posts need KaTeX flag" |
| Process improvement | Agent file | "Validator should check X" |
| Project constraint | `meta-reasoning-framework` | "Nix manages all deps" |

### Step 3: Format Update

```markdown
## Learning: [Title]

**Session**: [date/time]
**Category**: [coding/architecture/content/process/constraint]
**Trigger**: [what happened that led to this learning]

### The Pattern
[1-2 sentence description]

### Example
[Concrete code or scenario]

### Update Proposal
**File**: `.opencode/skill/<skill-name>/SKILL.md`
**Section**: [where to add]
**Add**:
```
[exact text to add]
```
```

---

## Memory Structure

```
.opencode/_memory/
├── session-memory.md      # Current session state
├── learnings/
│   ├── 2025-01-22.md      # Daily learnings
│   └── ...
├── patterns/
│   ├── coding.md          # Accumulated coding patterns
│   ├── architecture.md    # Architecture decisions
│   └── mistakes.md        # Common mistakes to avoid
└── metrics/
    └── improvement.md     # Track pattern effectiveness
```

---

## Automatic Triggers

| Trigger | Action |
|---------|--------|
| 3-strike rule activated | Extract what went wrong |
| Validator finds issue | Record the check that caught it |
| User says "good catch" | Extract what was caught |
| Session end | Summarize key learnings |

---

## Output Format

```markdown
## Session Learnings

**Date**: [timestamp]
**Duration**: [approx]
**Tasks Completed**: [list]

### Patterns Discovered

1. **[Pattern Name]**
   - Category: [L1/L2/L3]
   - Learning: [what we learned]
   - Update: [skill to update]

### Mistakes Made

1. **[Mistake]**
   - Root cause: [analysis]
   - Prevention: [how to avoid]
   - Skill update: [proposed change]

### Proposed Skill Updates

1. `[skill-name]` - Add: [what]
2. `[agent-name]` - Modify: [what]

### Memory Persisted

- Session summary → `.opencode/_memory/session-memory.md`
- Learnings → `.opencode/_memory/learnings/[date].md`
- Patterns → `.opencode/_memory/patterns/[category].md`
```

---

## Integration

**Invoked by**:
- `/learn` command (manual)
- Session end hook (automatic)
- Reflection skill (when fixing mistakes)

**Outputs to**:
- Memory files (immediate)
- Skill update proposals (for review)
- Metrics tracking (long-term)

**Feedback loop**:
```
Session → Learning → Skill Update → Better Next Session
```
