# Agent Orchestration System Design

> Final implementation aligned with OpenCode's skill discovery mechanism.

## Core Principle

**Agents are compositions of skills.**

OpenCode discovers skills via: `.opencode/skills/<skill-name>/SKILL.md`

All knowledge types are organized as skills under `.opencode/skills/` with a **flat structure**.

---

## The Three Knowledge Types

### 1. Fact Skills (Declarative - "What Is")

Constraints and rules that govern what is allowed or required.

| Skill | Purpose |
|-------|---------|
| `fact-project-constraints` | Bilingual, minimalist, Nix-only rules |
| `fact-haskell-patterns` | Haskell idioms for this project |

### 2. Core Skills (Procedural - "How To Do")

Procedures and workflows organized by layer:

| Skill | Layer | Purpose |
|-------|-------|---------|
| `skill-coding-standard` | L1 | Implementation standards |
| `skill-tdd-workflow` | L1 | Test-driven development |
| `fact-hakyll-architecture` | L2 | Design patterns |
| `skill-content-strategy` | L3 | Content decisions |
| `meta-verification-loop` | Cross-cutting | Verification procedures |
| `meta-continuous-learning` | Cross-cutting | Learning from sessions |
| `meta-reflection` | Cross-cutting | Session reflection |

### 3. Meta Skills (Epistemological - "How To Think")

Reasoning frameworks that guide decision-making.

| Skill | Purpose |
|-------|---------|
| `meta-reasoning-framework` | Layer tracing, 3-strike escalation |
| `meta-uncertainty-handling` | When to ask vs proceed |
| `meta-conflict-resolution` | Resolve skill/fact conflicts |

---

## Current Directory Structure

```
.opencode/
│
├── skill/                               # ALL discoverable skills (FLAT)
│   ├── fact-hakyll-architecture/SKILL.md
│   ├── fact-haskell-patterns/SKILL.md
│   ├── fact-project-constraints/SKILL.md
│   ├── meta-conflict-resolution/SKILL.md
│   ├── meta-continuous-learning/SKILL.md
│   ├── meta-reasoning-framework/SKILL.md
│   ├── meta-reflection/SKILL.md
│   ├── meta-uncertainty-handling/SKILL.md
│   ├── meta-verification-loop/SKILL.md
│   ├── skill-coding-standard/SKILL.md
│   ├── skill-content-strategy/SKILL.md
│   ├── skill-router/SKILL.md
│   └── skill-tdd-workflow/SKILL.md
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
name: meta-reasoning-framework
description: "META: Layer tracing and 3-strike rule"
type: meta
---
```

**Fact Skill**:
```yaml
---
name: fact-project-constraints
description: "FACT: Non-negotiable project rules"
type: fact
---
```

**Core Skill**:
```yaml
---
name: skill-coding-standard
description: "CORE: L1 implementation standards for Haskell/SCSS/Nix"
type: core
layer: L1
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

Each agent explicitly lists required skills in its markdown using the `/skill-name` syntax:

```markdown
---
name: coder
description: Implementation with verification
permission:
  skill:
    "*": allow
---

You are the **Coder**.

## Required Knowledge

**Load these skills:**
- `/fact-project-constraints` - Non-negotiable rules
- `/fact-haskell-patterns` - Language patterns
- `/skill-coding-standard` - Implementation standards
- `/meta-verification-loop` - How to verify
```

This makes dependencies explicit and documents what knowledge each agent needs.

---

## Invocation

Skills are invoked via the `/skill-name` syntax in chat or programmatically by agents using the `skill` tool.

**Skill Discovery Locations** (in order of precedence):
1. Project: `.opencode/skills/<name>/SKILL.md`
2. Global: `~/.config/opencode/skill/<name>/SKILL.md`

**Permission Control**: Agents can restrict skill access via `permission.skill` in frontmatter:

```yaml
permission:
  skill:
    "*": allow           # Default: allow all
    "internal-*": deny   # Block specific patterns
```

---

## Agent Composition Matrix

Each agent is a composition of skills from all three categories. This matrix shows exactly which skills each agent requires:

### Composition Overview

| Agent | FACT skills | CORE skills | META skills |
|------|------------|-----------|------------|
| Planner | `fact-project-constraints` | — | `meta-reasoning-framework`, `meta-uncertainty-handling` |
| Tech-Lead | `fact-project-constraints`, `fact-haskell-patterns` | `fact-hakyll-architecture`, `skill-content-strategy` | `meta-reasoning-framework`, `meta-conflict-resolution` |
| Coder | `fact-project-constraints`, `fact-haskell-patterns` | `skill-coding-standard`, `meta-verification-loop` | — |
| Validator | `fact-project-constraints` | `skill-coding-standard`, `meta-verification-loop` | — |
| Designer | `fact-project-constraints` | `skill-coding-standard`, `skill-content-strategy` | — |

### Detailed Agent Compositions

#### Planner (Orchestrator)

| Category | Skills | Purpose |
|----------|--------|---------|
| **FACT** | `fact-project-constraints` | Know what's allowed |
| **CORE** | — | Delegates execution to other agents |
| **META** | `meta-reasoning-framework`, `meta-uncertainty-handling` | Decide which layer, when to ask |

**Role**: Routes problems to the right agent by identifying the layer (L1/L2/L3).

---

#### Tech-Lead (Designer)

| Category | Skills | Purpose |
|----------|--------|---------|
| **FACT** | `fact-project-constraints`, `fact-haskell-patterns` | Know rules and language idioms |
| **CORE** | `fact-hakyll-architecture`, `skill-content-strategy` | Design at L2/L3 |
| **META** | `meta-reasoning-framework`, `meta-conflict-resolution` | Trace layers, resolve conflicts |

**Role**: Research and design. Produces design documents for Coder.

---

#### Coder (Implementer)

| Category | Skills | Purpose |
|----------|--------|---------|
| **FACT** | `fact-project-constraints`, `fact-haskell-patterns` | Know rules and patterns |
| **CORE** | `skill-coding-standard`, `meta-verification-loop` | Implement and verify at L1 |
| **META** | — | Follows instructions, doesn't need meta-reasoning |

**Role**: Implement designs. Produces code changes with verification.

---

#### Validator (Quality Gate)

| Category | Skills | Purpose |
|----------|--------|---------|
| **FACT** | `fact-project-constraints` | Know what's required |
| **CORE** | `skill-coding-standard`, `meta-verification-loop` | Check against standards |
| **META** | — | Evidence-based, doesn't need meta-reasoning |

**Role**: Verify implementations. Produces verdicts with evidence.

---

#### Designer (Visual/UX)

| Category | Skills | Purpose |
|----------|--------|---------|
| **FACT** | `fact-project-constraints` | Know minimalist requirements |
| **CORE** | `skill-coding-standard`, `skill-content-strategy` | SCSS patterns, content context |
| **META** | — | Follows design system, doesn't need meta-reasoning |

**Role**: Visual and UX design. Produces design specs for Coder.

---

### Composition Patterns

**Pattern 1: Meta skills for decision-makers**
- Planner and Tech-Lead have meta skills because they make routing/design decisions
- Coder, Validator, Designer follow instructions and don't need meta-reasoning

**Pattern 2: All agents share `fact-project-constraints`**
- Every agent must know the non-negotiable rules
- This ensures consistent behavior across the pipeline

**Pattern 3: Layer-specific core skills**
- L1 agents (Coder, Validator): `skill-coding-standard`, `meta-verification-loop`
- L2/L3 agents (Tech-Lead, Designer): `fact-hakyll-architecture`, `skill-content-strategy`

---

## Skill Naming Convention

We use **prefix-based skill names** so a flat skill directory stays self-organizing:

- `fact-*` = domain constraints / repository knowledge (**what is true**)
- `meta-*` = methodology / reasoning (**how to think**)
- `skill-*` = procedures / workflows (**how to do**)

This matches the convention used in `serenitea-pot` and makes skill lists easy to scan when sorted alphabetically.

### Examples

```yaml
# FACT skill
---
name: fact-project-constraints
description: "FACT: Non-negotiable project rules - bilingual, minimalist, Nix-only"
type: fact
---

# META skill
---
name: meta-reasoning-framework
description: "META: Layer tracing and 3-strike rule for problem solving"
type: meta
---

# CORE (procedural) skill
---
name: skill-coding-standard
description: "CORE: L1 implementation standards for Haskell/SCSS/Nix"
type: core
layer: L1
---
```

### Skill Catalog

| Skill | Category | Description |
|-------|----------|-------------|
| `fact-project-constraints` | FACT | Non-negotiable project rules |
| `fact-haskell-patterns` | FACT | Haskell idioms for this project |
| `fact-hakyll-architecture` | FACT | Hakyll architecture patterns (L2) |
| `meta-reasoning-framework` | META | Layer tracing, 3-strike escalation |
| `meta-uncertainty-handling` | META | When to ask vs proceed |
| `meta-conflict-resolution` | META | Resolve skill/fact conflicts |
| `meta-verification-loop` | META | Verification procedures (cross-cutting) |
| `meta-continuous-learning` | META | Learning from sessions (cross-cutting) |
| `meta-reflection` | META | Reflection / process improvement |
| `skill-coding-standard` | SKILL | Implementation standards (L1) |
| `skill-tdd-workflow` | SKILL | Test-driven development workflow (L1) |
| `skill-content-strategy` | SKILL | Content strategy decisions (L3) |
| `skill-router` | SKILL | Routing utility / layer classification |

---

## Design Rationale

### Why Everything as Skills?

1. **Discoverability**: OpenCode's mechanism requires `SKILL.md` files
2. **Uniformity**: One pattern for all knowledge types
3. **Selectivity**: Agents load only what they need
4. **Extensibility**: Easy to add new knowledge

### Why Flat Structure?

OpenCode expects skills at `.opencode/skills/<skill-name>/SKILL.md` - a flat structure where skill names are unique identifiers. This matches the invocation syntax `/skill-name` and ensures proper discovery.

Previous nested structures like `.opencode/skills/meta/reasoning-framework/SKILL.md` were not supported by OpenCode's discovery mechanism.

### Why Composition Matrix?

Explicit composition documentation:
1. **Clarity**: Know exactly what each agent can access
2. **Debugging**: When an agent fails, check if it has the right skills
3. **Onboarding**: New contributors understand the system quickly
4. **Evolution**: Easy to add/remove skills per agent
