---
name: reflection
description: Analyze agent failures and improve agent/skill definitions. Use when an agent missed something, gave wrong advice, or a workflow failed. Requires conversation history - must run in current session, not as subagent.
---

# Reflection

Analyze what went wrong and generate specific fixes to prevent recurrence.

**Important:** This is a skill, not an agent, because reflection requires full conversation history.

## When to Use

- Agent missed an obvious issue
- Agent gave advice that contradicted project context
- Same mistake happened twice
- Workflow produced wrong output

## Analysis Process

### Step 1: Reconstruct the Incident

Review the conversation history and identify:

```markdown
**Incident**: [what went wrong]
**Agent**: [which agent/skill was involved]
**Expected**: [what should have happened]
**Actual**: [what actually happened]
**Gap**: [what context or check was missing]
```

### Step 2: Identify Root Cause

| Category | Symptoms | Fix Type |
|----------|----------|----------|
| **Missing context** | Agent didn't know about project decisions | Add to agent's reference skills or inline knowledge |
| **Missing check** | Agent skipped a verification step | Add step to process |
| **Unclear instruction** | Agent misinterpreted guidance | Clarify wording |
| **Wrong assumptions** | Agent assumed without asking | Add "verify before assuming" rule |
| **Isolated session** | Agent couldn't see conversation | Consider converting to skill |

### Step 3: Generate Fix

Produce specific, minimal changes:

```markdown
**File**: `.opencode/agents/[name].md` or `.opencode/skills/[name]/SKILL.md`
**Section**: [which part to modify]

**Current**:
```
[existing text]
```

**Proposed**:
```
[new text]
```

**Why this prevents recurrence**:
[explanation]
```

## Quality Checklist

Good fixes are:
- [ ] **Specific** - Exact text changes, not vague "be more careful"
- [ ] **Minimal** - Smallest change that prevents the issue
- [ ] **Testable** - Can verify it would catch the same problem
- [ ] **Not punitive** - Improves systems, doesn't assign blame

Bad fixes:
- "Review more carefully" (not actionable)
- Adding 10 checks for one edge case (over-engineering)
- Blaming without proposing solution

## Output Format

```markdown
## Reflection: [Issue Title]

### Incident
[Brief description of what went wrong]

### Root Cause
**Category**: [from table above]
**Agent/Skill**: [which one]
**Gap**: [what was missing]

### Conversation Evidence
[Quote the specific messages that show the problem]

### Fix
**File**: [path]
**Change**: [specific edit]

### Verification
[How to confirm this fix works - e.g., "re-run the same prompt and check for X"]
```

## Example

```markdown
## Reflection: Tech-lead suggested unnecessary library change

### Incident
Tech-lead recommended switching from TagSoup to html-parse for "12x performance improvement" when testing only 50 HTML files.

### Root Cause
**Category**: Wrong assumptions
**Agent**: tech-lead
**Gap**: Agent optimized for scale without checking actual project size

### Conversation Evidence
> "Use `html-parse` instead of TagSoup... Performance: ~6 MB/s vs ~80 MB/s (12x faster)"

### Fix
**File**: `.opencode/agents/tech-lead.md`
**Change**: Add "Step 0: Understand Existing Decisions" section with rule "Consider project scale - optimizations for 10M files don't matter for 50 files"

### Verification
Ask tech-lead to review a small-scale testing design; should not suggest performance optimizations without measuring.
```
