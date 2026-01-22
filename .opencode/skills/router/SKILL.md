---
name: router
description: "ROUTING: Master router with 400+ keywords. Identifies layer, loads skills, routes to agents."
type: routing
---

# Router

> Identify entry layer → Load skills → Route to agent

## Automatic Invocation

This skill is auto-triggered by hooks on every user message. It:
1. Scans for keywords
2. Identifies entry layer
3. Loads appropriate skill(s)
4. Suggests agent routing

---

## Keyword Detection (400+ patterns)

### L1: Implementation Keywords

**Build/Compile Errors**
```
error, warning, fails, failed, E[0-9]+, GHC, cabal, nix build,
type mismatch, couldn't match, expected, actual, ambiguous,
not in scope, undefined, missing, import, module
```

**Haskell Mechanics**
```
type signature, instance, deriving, newtype, data, class,
monad, functor, applicative, IO, Maybe, Either, Text, String,
pattern match, guard, where, let, case, do notation,
ownership, borrow, move, reference, lifetime
```

**SCSS/CSS**
```
scss, css, style, color, font, margin, padding, border,
responsive, media query, flexbox, grid, variable, mixin,
animation, transition, hover, focus, dark mode, theme
```

**Nix**
```
flake, derivation, nix-shell, nix develop, nixpkgs,
package, dependency, build input, overlay
```

### L2: Architecture Keywords

**Structure/Design**
```
design, structure, architecture, pattern, approach,
where should, how to organize, refactor, restructure,
module, component, separation, coupling, cohesion
```

**Hakyll Specific**
```
route, template, compiler, context, rules, match,
snapshot, dependency, load, loadAll, makeItem,
pagination, feed, sitemap, metadata
```

**Patterns**
```
DRY, SOLID, separation of concerns, single responsibility,
composition, abstraction, interface, contract
```

### L3: Content/Feature Keywords

**Features**
```
feature, add, implement, support, enable, new,
want, need, should, could, would like
```

**Content**
```
reader, user, visitor, audience, UX, experience,
bilingual, i18n, language, translation, en, zh,
post, page, blog, article, content, write
```

**Strategy**
```
why, purpose, goal, objective, requirement,
SEO, accessibility, performance, speed, mobile
```

---

## Layer Detection Algorithm

```
function detectLayer(input):
    l1_score = count(L1_KEYWORDS in input)
    l2_score = count(L2_KEYWORDS in input)
    l3_score = count(L3_KEYWORDS in input)
    
    if l1_score > 0 AND contains_error_code(input):
        return L1, trace_up
    
    if l3_score > l2_score AND l3_score > l1_score:
        return L3, trace_down
    
    if l2_score > l1_score:
        return L2, trace_both
    
    return L1, trace_up
```

## Routing Table

| Entry Layer | Trace | Primary Agent | Skills to Load |
|-------------|-------|---------------|----------------|
| L1 (error) | UP ↑ | coder | `coding-standard` |
| L1 (refactor) | UP ↑ | coder | `coding-standard` + `hakyll-architecture` |
| L2 (design) | Both ↕ | tech-lead | `hakyll-architecture` |
| L2 (template) | Both ↕ | tech-lead | `hakyll-architecture` + `content-strategy` |
| L3 (feature) | DOWN ↓ | planner | `content-strategy` |
| L3 (bilingual) | DOWN ↓ | planner | `content-strategy` + `hakyll-architecture` |

## Dual-Skill Loading

When keywords from multiple layers appear:

| Combination | Load Both | Rationale |
|-------------|-----------|-----------|
| L1 + L3 | `coding-standard` + `content-strategy` | Error in feature context |
| L2 + L3 | `hakyll-architecture` + `content-strategy` | Design for feature |
| L1 + L2 | `coding-standard` + `hakyll-architecture` | Implementation of design |

---

## Output Format

```markdown
## Route Analysis

**Input**: [user message summary]

**Detected Keywords**:
- L1: [keywords found]
- L2: [keywords found]  
- L3: [keywords found]

**Entry Layer**: L[X]
**Trace Direction**: UP ↑ / DOWN ↓ / BOTH ↕

**Skills Loaded**:
1. `skill-name` - [why]
2. `skill-name` - [why]

**Recommended Agent**: [agent-name]

**Rationale**: [1 sentence]
```

---

## Special Cases

### Cross-Layer Problems
When problem spans layers, load skills from both and start at highest layer.

### Ambiguous Input
If no strong signal, ask clarifying question:
- "Is this about the content (what readers see) or implementation (how it's coded)?"

### Already Routed
If conversation already has layer context, don't re-route unless topic changes.

---

## Integration Points

**Triggered by**: `hooks.json` on every user message
**Outputs to**: Planner, Tech-Lead, or Coder
**Feedback to**: `continuous-learning` skill for pattern updates
