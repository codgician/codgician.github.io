---
name: skill-add-opencode-component
description: "CORE: Procedure to add a new OpenCode skill or subagent with strict permissions and fact/meta/skill composition."
type: core
---

# Add OpenCode Component (Skill or Subagent)

> This repo treats **agents as compositions of skills**.
>
> **Non-negotiables**:
> 1. Skills live at `.opencode/skill/<skill-name>/SKILL.md` (flat discovery)
> 2. Skill names are **prefixed**: `fact-*`, `meta-*`, `skill-*`
> 3. Subagents use **strict permissions**: `"*": deny` then explicit allowlists
> 4. A subagent must combine the **3 knowledge types**:
>    - **FACT**: what is true / constraints
>    - **META**: how to think / decide
>    - **SKILL**: how to do / procedures

---

## Part A — Add a New Skill

### Step 0: Classify the skill (FACT vs META vs SKILL)

Ask:

- **FACT** if it answers: "What is true in this repo? What are the constraints/patterns?"
  - Prefix: `fact-`
  - `type: fact`
  - Examples: `fact-project-constraints`, `fact-hakyll-architecture`

- **META** if it answers: "How should we reason when uncertain? How do we resolve conflicts?"
  - Prefix: `meta-`
  - `type: meta`
  - Examples: `meta-reasoning-framework`, `meta-verification-loop`

- **SKILL** if it answers: "How do we perform a workflow reliably?"
  - Prefix: `skill-`
  - `type: core`
  - Examples: `skill-coding-standard`, `skill-tdd-workflow`

If unsure: default to **SKILL** (procedures), unless it is a hard constraint (**FACT**) or a reasoning framework (**META**).

### Step 1: Choose the canonical name

Rule:

```
<category-prefix>-<domain>-<topic>

# Examples
fact-haskell-patterns
meta-uncertainty-handling
skill-add-opencode-component
```

Constraints:
- Lowercase
- Hyphen-separated
- Must be globally unique within `.opencode/skill/`

### Step 2: Create the directory + SKILL.md

Create:

```
.opencode/skill/<name>/SKILL.md
```

Frontmatter requirements:
- `name:` MUST equal the directory name
- `description:` should start with `FACT:` / `META:` / `CORE:`
- `type:` should match the category (`fact` / `meta` / `core`)
- Optional: `layer: L1|L2|L3` for execution skills

### Step 3: Update docs & discovery metadata

Update `.opencode/DESIGN.md`:
- Add the skill to the **Skill Catalog**
- If relevant, update composition tables

### Step 4: Wire the new skill into the system

Choose the integration points:

1. **Agent usage**: if an agent should load it
   - Update the agent’s **Required Knowledge** section
   - Update the agent’s `permission.skill` allowlist

2. **Command usage**: if it should be invokable via `/command`
   - Create or update `.opencode/commands/<cmd>.md`
   - Set `skill: <skill-name>` in command frontmatter

3. **Router usage**: if it should be suggested for certain inputs
   - Update `.opencode/skill/skill-router/SKILL.md` mapping tables

### Step 5: Verification (skill)

Run:

```bash
# skill exists
test -f ".opencode/skill/<name>/SKILL.md"

# no stale references
rg "\.opencode/skills" .opencode

# build
nix build
```

---

## Part B — Add a New Subagent

### Step 0: Decide the agent’s responsibility

A new subagent is justified when:
- It has a clear, repeatable role (review, research, docs, design, etc.)
- The role can be expressed as a **specific skill allowlist**

### Step 1: Define the 3-type skill composition

**Every subagent must have:**
- ≥1 FACT skill (constraints/patterns)
- ≥1 META skill (reasoning/verification/conflict handling)
- ≥1 SKILL/core skill (procedures)

Example composition patterns:

- Reviewer:
  - FACT: `fact-project-constraints`
  - META: `meta-verification-loop`
  - SKILL: `skill-coding-standard`

- Researcher:
  - FACT: `fact-project-constraints`, `fact-hakyll-architecture`
  - META: `meta-uncertainty-handling`
  - SKILL: `skill-router`

### Step 2: Create the agent file

Create:

```
.opencode/agents/<agent-name>.md
```

Frontmatter checklist:
- `name:` and `description:`
- `mode: subagent`
- `model:` set to your desired dendro endpoint

### Step 3: Implement strict permissions

#### Skill permissions (mandatory)

```yaml
permission:
  skill:
    "*": deny
    "fact-...": allow
    "meta-...": allow
    "skill-...": allow
```

Only allow the minimum set needed.

#### Tool permissions (recommended)

- Prefer `edit: deny` for non-implementers (research/review roles)
- Consider denying destructive bash patterns (optional hardening):

```yaml
permission:
  bash:
    "rm *": deny
    "git push*": deny
```

### Step 4: Add Required Knowledge section

Document the intended composition explicitly:

```markdown
## Required Knowledge

**Load these skills (fact + skill + meta):**
- `/fact-...`
- `/skill-...`
- `/meta-...`
```

### Step 5: Integrate into Planner spawning rules

If the Planner should be able to spawn this agent:
- Update `.opencode/agents/planner.md`:

```yaml
permission:
  task:
    "*": deny
    "<agent-name>": allow
```

### Step 6: Update docs

Update `.opencode/DESIGN.md`:
- Add the agent to the composition table
- Mention when the agent should be used

### Step 7: Verification (agent)

```bash
# agent file exists
test -f ".opencode/agents/<agent-name>.md"

# skill allowlist entries exist
# (spot check quickly)
rg '"[a-z-]+": allow' .opencode/agents/<agent-name>.md

# build
nix build
```

---

## Quick Checklists

### Skill Checklist
- [ ] Name uses correct prefix: `fact-` / `meta-` / `skill-`
- [ ] Directory name == frontmatter `name:`
- [ ] `type:` matches category
- [ ] Added to `.opencode/DESIGN.md`
- [ ] If referenced by an agent, the agent permission allowlist is updated

### Subagent Checklist
- [ ] Role is crisp and non-overlapping
- [ ] Has fact + meta + skill composition
- [ ] `permission.skill` is deny-by-default
- [ ] Required Knowledge lists only allowed skills
- [ ] Planner `task` allowlist updated if needed
- [ ] `nix build` passes
