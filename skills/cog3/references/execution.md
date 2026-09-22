# Phases 4–5 — Autoresearch Execution & Finalize (+ Completion)

## Phase 4: Autoresearch Execution

Execute handoffs in dependency order. Each approved `.feature`+`.md` pair is the complete task boundary. Before editing, read both files and activate recorded adapters (or preserve the discipline natively). **Do not** pull unrelated scenarios into the task.

Per handoff: **build & review the TODO skeleton once before the loop** (a checkpoint — see `autoresearch-loop.md`), then the loop is **autonomous within `max_cycles`** (no per-cycle permission, **no per-cycle review**) with exactly **one** mid-loop stop: the atomicity-split re-approval. **Orchestrate via workflows where available:** run **dependency-free handoffs as parallel workflows** (each in its own `.loop/`, scopes disjoint), and within a cycle fan out candidate fills by angle. Respect the handoff dependency DAG; never parallelize a handoff with one it depends on. For every handoff, run the **per-handoff autoresearch loop** (full mechanics in `autoresearch-loop.md`):
1. Confirm exactly one focused scenario and that dependencies are green; resolve unavailable adapters to an equivalent capability or native instructions.
2. **Recheck the Atomicity Proof against repository reality.** If implementation reveals another independently verifiable behavioral delta, **stop, split the approved handoff, and obtain approval for the changed plan before production edits.**
3. Confirm assertions and properties use the repo's real framework with concrete expected values and a real generator. For debugging handoffs, confirm reproduction is reliable, fail path traced, hypothesis survived disproof, breadcrumbs consistent.
4. **Plan & review, then run the loop:** write property tests + seed examples (Gherkin scenario among them) → **build the TODO skeleton** (structures+connections → stubbed interfaces → TODO change-sites *tagged with the property they feed / example they satisfy*, inserted in code → break/revert points → invariants) → **run the suite = behavioral Red baseline** (skeleton compiles, so failures are missing logic) → **verify the TODO grep, then request a review** (the per-handoff checkpoint before the loop; apply its fixes, re-verify) → then the loop **only fills** those TODOs: generate-test-score-mutate (mutate-from-best, score on a fixed suite, freeze each counterexample, keep/discard on the per-test vector) → stabilize across 3 seeds → bounded refactor-green. Markers removed on green, none left behind. The loop yields one candidate per cycle and keeps/discards on the full suite (see `autoresearch-loop.md`). Autonomous within `max_cycles`; checkpoints every 10, no per-cycle permission or review.
5. Record into the handoff `.md`: Red evidence, property + example pass vectors, counterexamples frozen, seeds, files changed, deviations (and, for debugging, final reproduction, root-cause evidence, falsification, full ledger). Advance the goal ledger.

Never batch several unobserved Red tests with a large implementation — one handoff's loop at a time. If repo constraints make the discipline impossible, stop and explain the exact constraint rather than claiming compliance.

## Phase 5: Simplification and Architecture Review

Enter only after every handoff is green and stable and its regression checks pass. The loop already refactored *within* each handoff; this is the **cross-handoff** pass over all Cog3-touched code.

**Simplify touched code** — activate `code-simplifier` if installed, else apply directly. Limit to production+test code modified during this run. Preserve all behavior, outputs, contracts, and passing assertions/properties. Reduce needless nesting, duplication, indirection, comments, abstractions only when clarity improves; prefer explicit control flow; keep useful abstractions. Rerun focused tests for every changed handoff **and the property suites** plus relevant regression after simplifying; revert anything that changes behavior.

**Review touched architecture** — activate `beautify` if installed, else apply a bounded review. Read `CONTEXT.md` and relevant `docs/adr/` when present. Inspect only modules touched by this run and their immediate callers. Use the vocabulary `module/interface/implementation/depth/seam/adapter/leverage/locality`; apply the deletion test to suspected shallow modules; treat the interface as the test surface; don't recommend a seam backed by one adapter unless future variation is an approved requirement. Record each real deepening opportunity (files/modules, interface or locality problem, proposed deepening, benefits, ADR conflicts) as numbered follow-ups. Do not implement unapproved architectural expansion. Architecture findings don't block completion unless they reveal an unmet acceptance criterion, invariant, or regression guarantee.

## Completion

Finish only when:
- every approved Gherkin scenario maps to a passing executable test, and **every handoff's properties are green and stable across 3 seeds**;
- every task has one approved `.feature`/`.md` handoff with properties + seed examples, names its execution discipline, and records provider-resolvable adapters;
- every handoff has a credible Atomicity Proof and survived both the planning split test and the pre-execution split check;
- every handoff's exact assertions pass, and its Red evidence shows at least one assertion/property failed for the intended reason;
- all task verification commands and relevant regression checks pass;
- counterexamples frozen during the loop are recorded and remain green;
- **no `TODO(` markers remain in any handoff scope** (all filled and removed);
- deviations, budget-exhausted handoffs, and parked risks are reported;
- debugging tasks record reliable reproduction, traced fail path, falsified-and-surviving root cause, complete ledger, and removed instrumentation;
- the final simplification pass covers only Cog3-touched code, preserves behavior, and passes focused + property + regression verification;
- the touched architecture is reviewed; findings reported without unapproved scope expansion;
- no required work remains.

Use the execution report in `templates.md`. Mark the goal complete through the selected adapter only after these conditions hold. Report provider usage metrics only when the provider returns them. If genuinely blocked, keep status `active` until the adapter's documented blocked threshold; never mark incomplete work complete.
