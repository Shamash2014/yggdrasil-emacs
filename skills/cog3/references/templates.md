# Cog3 templates

Use these at phase gates and in the final report. They extend the Cog2 two-file handoff with a **property metric** so each handoff can be driven by the autoresearch loop.

## Goal Ledger (portable fallback)

```
GOAL: <objective>
Status: active | blocked | complete
Phase: 1 Converge | 2 Explore | 3 Decompose | 4 Autoresearch execution | 5 Simplify+Arch
Handoffs completed: <list>
Handoffs pending:   <list>
Blockers: <list or none>
```
Repeat at every phase gate and in the final report. After repo editing is approved, persist at `docs/cog3/<plan-slug>/goal.md`.

## Shared-understanding summary (Phase 1 gate)

```
OUTCOME: <observable success>
ACTORS / ENTRY POINTS: ...
SCOPE / NON-GOALS: ...
INPUTS / OUTPUTS / STATE / INVARIANTS: ...
HAPPY PATH / EDGES / FAILURE BEHAVIOR: ...
ACCEPTANCE CRITERIA: ...
PARKED (with risk): ...
Confirm or correct before I inspect the repository.
```

## Evidence map (Phase 2)

```
Affected behavior & current flow:
Likely files & ownership boundaries:
Reusable patterns & test infrastructure:
Property framework + seed flag + per-test reporter:   <- needed by the loop
Constraints discovered in code:
Plan↔repo contradictions:
Risks / open questions:
(debugging) reproduction cmd & rate, fail path, knobs, ranked hypotheses, disproof, ledger:
```

## Handoff — two files + property metric (Phase 3)

`<NN>-<slug>.feature` — exactly one `Feature`, one focused `Scenario`/`Scenario Outline`. This scenario is a **seed example** for the loop.

`<NN>-<slug>.md`:
```
Task: <NN>-<slug>
Outcome (one behavioral delta): ...
Dependencies: <earlier handoffs that must be green>
Repository evidence: <files, patterns, contracts>
Surface: <function/module/endpoint the loop may modify>

PROPERTIES (acceptance metric — ALL must be green, stable across 3 seeds):
  P1. <universal invariant> — <archetype: roundtrip|invariant|oracle|idempotence|metamorphic|order|error-contract> — generator: <inputs>
  P2. ...
SEED EXAMPLES (gradient; the .feature scenario is E1):
  E1. <from .feature> — <input → expected>
  E2. <edge/boundary/repro> — <input → expected>

Exact assertions: <subject, matcher, expected value, Red failure/diff expected>
Red test & expected failure: ...
Minimal green target: ...
Verification commands: <test_cmd + per-test reporter>
Non-goals: ...
Atomicity Proof: <the single behavioral delta; why no smaller useful red-green-refactor exists>
Required execution discipline: autoresearch loop (property-metric red-green-refactor) — see references/autoresearch-loop.md
TDD adapter: <provider-native skill id | native instructions>
Specialist adapter: <provider-native skill id | available capability | none>
Completion evidence (filled in Phase 4): red evidence, property+example vectors, counterexamples frozen, seeds, files changed, deviations
(debugging) reproduction evidence, fail-path evidence, accepted root-cause hypothesis, falsification, experiment ledger
```

## Execution report (Completion)

```
GOAL: <objective> — COMPLETE
Handoffs: <N> (all properties green, stable across 3 seeds)
Per handoff: properties <g>/<P>, examples <E_start>→<E> (counterexamples frozen: <k>), cycles run, budget status
Gherkin scenarios → passing tests: <map>
Simplification pass (Cog3-touched code only): refinements + verification
Architecture review (touched area): numbered deepening follow-ups (not implemented)
Deviations / parked risks: ...
(debugging) reproduction, root cause, falsification, ledger, instrumentation removed
```
