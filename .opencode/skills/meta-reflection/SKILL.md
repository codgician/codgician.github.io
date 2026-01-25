---
name: meta-reflection
description: "META: Analyze failures and improve the system. 5-question reboot for when things go wrong."
type: meta
---

# Reflection

> Analyze what went wrong and fix the system, not just this instance.

**Important**: This skill needs conversation history to work effectively.

---

## 5-Question Reboot

| # | Question | Purpose |
|---|----------|---------|
| 1 | What were we trying to achieve? | Goal clarity |
| 2 | What actually happened? | Symptom identification |
| 3 | What layer was the problem in? | Layer diagnosis |
| 4 | What was assumed vs. verified? | Gap identification |
| 5 | How do we prevent this systemically? | System improvement |

---

## Root Cause Categories

| Category | Typical Fix |
|----------|-------------|
| **Wrong layer** | Add trace direction guidance to router |
| **Missing check** | Add to verification-loop |
| **Wrong assumption** | Add "verify before assuming" rule |
| **Missing context** | Add to agent's handoff template |
| **Pattern mismatch** | Add example to coding-standard |

---

## Analysis Process

### Step 1: Symptom Capture
```markdown
**What happened**: [describe the failure]
**When**: [during which step/agent]
**Impact**: [what went wrong as a result]
```

### Step 2: Layer Diagnosis
```
Was this a...
- L1 issue? (wrong implementation)
- L2 issue? (wrong design)
- L3 issue? (wrong understanding of needs)
- Cross-layer? (miscommunication between layers)
```

### Step 3: Gap Analysis
```markdown
**Assumed**: [what we thought was true]
**Reality**: [what was actually true]
**Gap**: [the difference]
**Detection**: [how we could have caught this earlier]
```

### Step 4: Systemic Fix
```markdown
**File to update**: `.opencode/[path]`
**Section**: [where in the file]
**Change**: [what to add/modify]
**Prevents**: [how this stops recurrence]
```

---

## Output Format

```markdown
## Reflection: [Issue Title]

### 5-Question Analysis

1. **Goal**: [what we wanted to achieve]
2. **Happened**: [the failure/mistake]
3. **Layer**: L[X] - [explanation]
4. **Gap**: Assumed [X], should have verified [Y]
5. **Fix**: [systemic change needed]

### Root Cause
**Category**: [wrong layer / missing check / wrong assumption / etc.]
**Details**: [explanation]

### Proposed Updates

#### Update 1: [file]
**File**: `.opencode/[path]`
**Section**: [section name]
**Add**:
```
[exact text to add]
```
**Rationale**: [why this prevents recurrence]

### Verification
After applying these updates, this type of issue should be caught by:
- [ ] Router detecting [keyword]
- [ ] Validator checking [condition]
- [ ] Agent [name] asking [question]
```

---

## Integration with Continuous Learning

After reflection, invoke `meta-continuous-learning` skill to:
1. Persist the learning to memory
2. Track pattern effectiveness over time
3. Update relevant skills with new patterns

```
Reflection → Proposed Fix → Apply → Continuous Learning → Persist
```

---

## Common Reflection Patterns

### "Build kept failing"
- **Likely cause**: Implementation without verification loop
- **Fix**: Add checkpoint after each change

### "Wrong file was modified"
- **Likely cause**: Didn't search for existing patterns
- **Fix**: Add "grep first" to coder instructions

### "Feature didn't match requirements"
- **Likely cause**: Skipped L3 analysis
- **Fix**: Add constraint check to tech-lead

### "Same error 3+ times"
- **Likely cause**: Wrong layer
- **Fix**: 3-strike rule wasn't followed, add reminder
