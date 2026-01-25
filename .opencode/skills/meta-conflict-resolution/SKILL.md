---
name: meta-conflict-resolution
description: "META: Resolve conflicts between skills, facts, or agent recommendations using precedence rules."
type: meta
---

# Conflict Resolution

> When knowledge sources disagree, use precedence rules to decide.

## Precedence Hierarchy

```
1. Platform Schema (HIGHEST)
   └── Hard limits: permissions, tools, model
   
2. Project Facts
   └── Constraints that filter options
   
3. Meta Skills
   └── Reasoning frameworks that guide approach
   
4. Core Skills (LOWEST)
   └── Procedures that execute within constraints
```

**Rule**: Higher layers constrain lower layers. Lower layers cannot override higher.

## Conflict Types

### Type 1: Fact vs. Skill

**Scenario**: Skill says "use pattern X" but Fact says "X is forbidden"

**Resolution**: **Fact wins**. Skills are guidance; Facts are constraints.

```
Example:
  Skill (coding-standard): "Use error for impossible cases"
  Fact (project/constraints): "Never use error, always Maybe/Either"
  
  Resolution: Use Maybe/Either (Fact wins)
```

### Type 2: Skill vs. Skill

**Scenario**: Two skills give conflicting guidance

**Resolution**: 
1. Check if one is more specific (specific wins)
2. Check which layer the task is in (use that layer's skill)
3. If still ambiguous, apply uncertainty-handling → Ask

```
Example:
  Skill A (coding-standard): "Keep functions under 20 lines"
  Skill B (hakyll-patterns): "This compiler pattern is typically 30 lines"
  
  Resolution: Skill B wins (more specific to context)
```

### Type 3: Agent vs. Agent

**Scenario**: Tech-lead design conflicts with coder implementation concern

**Resolution**: 
1. Tech-lead owns L2 decisions
2. Coder escalates L1 concerns with evidence
3. If design is proven unworkable (3-strike), reassess at L2

```
Example:
  Tech-lead: "Use polymorphic function"
  Coder: "Type system won't allow this after 3 attempts"
  
  Resolution: Escalate to Tech-lead with evidence, reassess design
```

### Type 4: Meta vs. Core

**Scenario**: Meta skill says "ask" but Core skill has a default answer

**Resolution**: **Meta wins**. Meta-cognition governs when to apply core skills.

```
Example:
  Meta (uncertainty): "Low confidence + irreversible → Ask"
  Core (coding-standard): "Default to Text over String"
  Task: Choosing type for new public API
  
  Resolution: Ask (meta-cognition overrides default)
```

## Resolution Process

```
1. Identify conflict type
2. Check precedence hierarchy
3. Apply type-specific rule
4. If still ambiguous:
   a. Gather more context
   b. Apply uncertainty-handling
   c. Document decision and rationale
```

## Documentation Pattern

When resolving conflicts, document:

```markdown
## Conflict Resolution

**Conflict**: [description]
**Sources**: 
  - [Source A]: [what it says]
  - [Source B]: [what it says]
**Type**: Fact vs Skill / Skill vs Skill / etc.
**Resolution**: [what was decided]
**Rationale**: [why, citing precedence rule]
```

## Edge Cases

### Facts That Span Categories

Some knowledge is both Fact and Skill:
- "Nix is truth" = Fact (constraint) + Skill (always run `nix build`)

**Resolution**: The Fact aspect (constraint) takes precedence. The Skill aspect (procedure) is how to satisfy the constraint.

### Contextual Override

Sometimes context changes precedence:
- In `dev` context: Speed > Perfection
- In `review` context: Correctness > Speed

**Resolution**: Context modifies how strictly to apply rules, but doesn't change the hierarchy.

### New Information

If execution reveals a Fact is wrong:
1. Don't silently override
2. Flag: "Discovered: [X]. Current fact says [Y]. Should update?"
3. Continue with new information, note the discrepancy

## Anti-Patterns

| Don't | Why | Instead |
|-------|-----|---------|
| Silently ignore conflicts | Hides problems | Explicitly resolve |
| Always ask | Wastes time | Use precedence |
| Let lower override higher | Breaks system | Enforce hierarchy |
| Change facts without flagging | Loses knowledge | Document updates |
