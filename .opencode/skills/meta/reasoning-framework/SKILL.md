---
name: reasoning-framework
description: "META: Layer tracing and 3-strike rule for problem solving. Load this to understand HOW TO THINK about problems."
type: meta
---

# Meta-Cognition Framework

> Don't answer directly. Trace through cognitive layers first.

## The Three Layers

```
L3: Content (WHY)       → What does the reader need?
    │
    ▼
L2: Architecture (WHAT) → How should it be structured?
    │
    ▼
L1: Implementation (HOW) → How do we code it?
```

## Trace Directions

| Entry Point | Direction | Example |
|-------------|-----------|---------|
| L1 error | UP ↑ | Build fails → Design issue? → Content mismatch? |
| L2 design | Both ↕ | Check constraints (↑) → Implement (↓) |
| L3 feature | DOWN ↓ | Feature → Structure → Code |

## The 3-Strike Rule

After 3 failed attempts at same layer → **STOP** → Trace to adjacent layer.

```
Strike 1: Try direct fix
Strike 2: Try alternative approach  
Strike 3: STOP - You're at wrong layer
         └─→ Trace UP or DOWN
```

## 5-Question Reboot (When Stuck)

| # | Question | Purpose |
|---|----------|---------|
| 1 | What am I trying to achieve? | Goal clarity |
| 2 | What layer is this? | Layer identification |
| 3 | What constraints apply? | L3 check |
| 4 | What have I tried? | Pattern recognition |
| 5 | Should I trace UP or DOWN? | Direction decision |

---

## Concrete Examples

### Example 1: Build Error (Entry: L1)

```
User: "Build fails with E0382 ownership error in Site.hs"

Trace UP:
├── L1: What's the immediate error? → Move after borrow
│       ↑
├── L2: Why is this data passed around? → Shared across functions
│       ↑
└── L3: What's the data's role? → Post metadata, immutable

Decision: Use reference, not ownership transfer
```

### Example 2: New Feature (Entry: L3)

```
User: "Add dark mode support"

Trace DOWN:
├── L3: What does reader need? → System preference, manual toggle
│       ↓
├── L2: How to structure? → CSS variables, prefers-color-scheme
│       ↓
└── L1: How to implement? → SCSS variables, JS toggle, localStorage

Decision: CSS-first with JS enhancement
```

### Example 3: Design Question (Entry: L2)

```
User: "Where should pagination logic live?"

Trace BOTH:
├── L3 ↑: What content patterns need pagination? → Posts, slides
├── L2: Current structure? → Posts has it, slides doesn't
└── L1 ↓: How to generalize? → Extract to Paginate.hs

Decision: Shared module with configurable URLs
```

---

## Layer-Skill Mapping

| Layer | Focus | Primary Skills |
|-------|-------|----------------|
| L3 | Why (Content) | `content-strategy`, `reflection` |
| L2 | What (Structure) | `hakyll-architecture`, `tdd-workflow` |
| L1 | How (Code) | `coding-standard`, `verification-loop` |

## Cross-Layer Keywords

| Keywords | Likely Layers | Load Skills |
|----------|---------------|-------------|
| bilingual + error | L3 + L1 | `content-strategy` + `coding-standard` |
| template + feature | L2 + L3 | `hakyll-architecture` + `content-strategy` |
| refactor + design | L1 + L2 | `coding-standard` + `hakyll-architecture` |

---

## Integration with Agents

```
┌─────────────────────────────────────────┐
│              User Input                  │
└─────────────────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────┐
│         Router (Auto-triggered)          │
│  • Detect keywords (400+ patterns)       │
│  • Identify entry layer                  │
│  • Load appropriate skills               │
└─────────────────────────────────────────┘
                    │
        ┌───────────┼───────────┐
        ▼           ▼           ▼
   ┌────────┐  ┌────────┐  ┌────────┐
   │ L3     │  │ L2     │  │ L1     │
   │ Planner│  │Tech-   │  │ Coder  │
   │        │  │Lead    │  │        │
   └────────┘  └────────┘  └────────┘
        │           │           │
        └───────────┼───────────┘
                    ▼
┌─────────────────────────────────────────┐
│            Validator                     │
│  • Evidence-based verification           │
│  • Cross-layer consistency check         │
│  • Final verdict                         │
└─────────────────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────┐
│        Continuous Learning               │
│  • Extract patterns from session         │
│  • Update skills if needed               │
│  • Persist memory for next session       │
└─────────────────────────────────────────┘
```
