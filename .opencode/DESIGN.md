# Agent Orchestration System Design

> Final implementation aligned with OpenCode's skill discovery mechanism.

## Core Principle

**Agents are compositions of skills.**

OpenCode discovers skills via: `{skill,skills}/**/SKILL.md`

Therefore, ALL knowledge types are organized as skills under `.opencode/skills/`.

---

## The Three Knowledge Types

### 1. Fact Skills (Declarative - "What Is")

**Location**: `.opencode/skills/facts/`

Constraints and rules that govern what is allowed or required.

| Skill | Purpose |
|-------|---------|
| `facts/project-constraints` | Bilingual, minimalist, Nix-only rules |
| `facts/haskell-patterns` | Haskell idioms for this project |

### 2. Core Skills (Procedural - "How To Do")

**Locations**: `.opencode/skills/{coding,architecture,process}/`

Procedures and workflows organized by layer:

| Category | Skills | Layer |
|----------|--------|-------|
| `coding/` | `coding-standard`, `tdd-workflow` | L1 (Implementation) |
| `architecture/` | `hakyll-architecture`, `content-strategy` | L2/L3 (Structure/Content) |
| `process/` | `verification-loop`, `continuous-learning`, `reflection` | Cross-cutting |

### 3. Meta Skills (Epistemological - "How To Think")

**Location**: `.opencode/skills/meta/`

Reasoning frameworks that guide decision-making.

| Skill | Purpose |
|-------|---------|
| `meta/reasoning-framework` | Layer tracing, 3-strike escalation |
| `meta/uncertainty-handling` | When to ask vs proceed |
| `meta/conflict-resolution` | Resolve skill/fact conflicts |

---

## Current Directory Structure

```
.opencode/
│
├── skills/                              # ALL discoverable knowledge
│   │
│   ├── meta/                            # META SKILLS (How to Think)
│   │   ├── reasoning-framework/SKILL.md
│   │   ├── uncertainty-handling/SKILL.md
│   │   └── conflict-resolution/SKILL.md
│   │
│   ├── facts/                           # FACT SKILLS (What Is)
│   │   ├── project-constraints/SKILL.md
│   │   └── haskell-patterns/SKILL.md
│   │
│   ├── coding/                          # CODING SKILLS (L1)
│   │   ├── coding-standard/SKILL.md
│   │   └── tdd-workflow/SKILL.md
│   │
│   ├── architecture/                    # ARCHITECTURE SKILLS (L2/L3)
│   │   ├── hakyll-architecture/SKILL.md
│   │   └── content-strategy/SKILL.md
│   │
│   ├── process/                         # PROCESS SKILLS (Cross-cutting)
│   │   ├── verification-loop/SKILL.md
│   │   ├── continuous-learning/SKILL.md
│   │   └── reflection/SKILL.md
│   │
│   └── router/SKILL.md                  # ROUTING SKILL
│
├── agents/                              # COMPOSED AGENTS
│   ├── planner.md                       # Orchestration, routing
│   ├── tech-lead.md                     # Research + design
│   ├── coder.md                         # Implementation
│   ├── validator.md                     # Quality gate
│   └── designer.md                      # Visual/UX
│
├── commands/                            # USER COMMANDS
│   └── *.md
│
├── contexts/                            # MODE SWITCHING
│   └── *.md
│
├── hooks.json                           # AUTOMATION
│
├── _memory/                             # RUNTIME STATE
│   └── *.md
│
└── DESIGN.md                            # This file
```

---

## Skill Frontmatter Convention

Each skill uses YAML frontmatter to indicate its type:

```yaml
---
name: skill-name
description: "TYPE: Brief purpose description"
type: meta | fact | core | routing
layer: L1 | L2 | L3  # Optional, for core skills
---
```

### Examples

**Meta Skill**:
```yaml
---
name: reasoning-framework
description: "META: Layer tracing and 3-strike rule"
type: meta
---
```

**Fact Skill**:
```yaml
---
name: project-constraints
description: "FACT: Non-negotiable project rules"
type: fact
---
```

**Core Skill**:
```yaml
---
name: hakyll-architecture
description: "CORE: L2 architecture patterns for Hakyll"
type: core
layer: L2
---
```

---

## Agent Composition Formula

```
Agent = Platform Schema (capabilities)
      + Fact Skills (constraints)
      + Meta Skills (reasoning)
      + Core Skills (execution)
```

### Precedence Hierarchy

When conflicts arise, follow this order:

1. **Platform Schema** → Hard limits (what's possible)
2. **Fact Skills** → Constraints (what's allowed)
3. **Meta Skills** → Reasoning (how to decide)
4. **Core Skills** → Execution (how to do)

---

## How Agents Reference Skills

Each agent explicitly lists required skills in its markdown:

```markdown
---
name: coder
description: Implementation with verification
---

You are the **Coder**.

## Required Knowledge

**Load these skills:**
- `/facts/project-constraints` - Non-negotiable rules
- `/facts/haskell-patterns` - Language patterns
- `/coding/coding-standard` - Implementation standards
- `/process/verification-loop` - How to verify
```

This makes dependencies explicit and documents what knowledge each agent needs.

---

## Invocation

Skills can be invoked via:
- `/skill-name` syntax in chat
- Programmatic loading by agents
- Automatic discovery by OpenCode

---

## Design Rationale

### Why Everything as Skills?

1. **Discoverability**: OpenCode's mechanism requires `SKILL.md` files
2. **Uniformity**: One pattern for all knowledge types
3. **Selectivity**: Agents load only what they need
4. **Extensibility**: Easy to add new knowledge

### Why Directory Prefixes?

The `meta/`, `facts/`, `coding/`, etc. prefixes provide:
- Visual organization in the file system
- Semantic hints about content type
- Natural grouping for related knowledge

They're **naming conventions**, not different mechanisms.
