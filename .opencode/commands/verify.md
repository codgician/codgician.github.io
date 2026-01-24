---
description: Run full verification suite with evidence
skill: meta-verification-loop
---

## Full Verification

Run complete verification suite and produce evidence-based report.

**Checks to run**:
1. `nix build` - Build passes
2. `cabal test` - Tests pass
3. `hlint src/` - No hints
4. Layer-specific checks (L1/L2/L3)

**Context**: $ARGUMENTS

Load the `meta-verification-loop` skill and:
1. Run all verification commands
2. Capture output as evidence
3. Produce verification report
4. Give verdict: ✅ VERIFIED / 🟡 PARTIAL / 🔴 FAILED
