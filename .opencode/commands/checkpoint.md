---
description: Save current state as a checkpoint for rollback/resume
skill: meta-verification-loop
---

## Checkpoint: $ARGUMENTS

Create a checkpoint of current work state.

**Actions**:
1. Run quick verification (`nix build`)
2. Record current git state
3. Document what was being worked on
4. Note next steps

**Output checkpoint to**: `.opencode/_memory/checkpoints/`

Load the `meta-verification-loop` skill and create a checkpoint with:
- Build status
- Current changes (staged/unstaged)
- Task context
- Rollback instructions
