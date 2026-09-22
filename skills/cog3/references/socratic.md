# Phases 1–2 — Socratic Convergence & Codebase Exploration (+ Debug Mantra)

## Phase 1: Socratic Convergence

Start from the user's informal plan (ask if none). Silently build a decision tree covering: desired outcome and observable success; users/actors/entry points; scope and non-goals; inputs/outputs/state/invariants; happy path/edges/failure behavior; compatibility/migration/rollout/rollback; security/privacy/performance/operational constraints when relevant; verification and acceptance criteria.

**Interview discipline:** ask **one question per user turn**. Lead with questions that expose assumptions, consequences, evidence, and edge cases; don't answer the design question for the user when a question can help them derive it. Resolve one branch depth-first before opening another. Treat vague answers as unresolved — ask for an example, invariant, threshold, failure policy, or explicit tradeoff. Never invent requirements; record unresolved items as parked, with their risk.

Choose the highest-risk unresolved branch and ask **one** concise question (assumption? distinguishing observable? boundary/failure? what must stay unchanged? why over the strongest alternative? what would falsify it?). After each answer: restate the resolved decision, note any dependency unlocked, ask the next single question, periodically report resolved/unresolved/parked counts.

When all material branches are resolved or parked, present the **shared-understanding summary** (`templates.md`). Ask the user to confirm or correct. **Gate: do not enter Phase 2 without explicit confirmation, and do not inspect or edit the repo before it.**

## Phase 2: Explore the Codebase

After confirmation, inspect before designing. **Use a workflow / parallel sub-agents** where available: fan out mappers over distinct subsystems and synthesize into the evidence map (fall back to inline when absent).
1. Read repo guidance; determine language, framework, build, **test command, per-test reporter (JSON/JUnit/TAP), lint, format**.
2. Identify the **property-based test framework** and **how to pin its seed** (Hypothesis `--hypothesis-seed`/`.hypothesis/`, fast-check `{seed}`, proptest `proptest-regressions/`, rapid `-rapid.seed`, etc.); if none is installed, adding it is a setup step.
3. Map entry points, modules, callers/callees, data flow, state transitions, side effects, public contracts. Read relevant files end-to-end incl. neighboring tests/fixtures. Search analogous implementations and local patterns. Inspect history when intent is unclear. Check worktree status and preserve unrelated changes. Run the narrowest useful baseline tests when practical.

For debugging, exploration also produces the runnable reproduction, fail-path trace, knob inventory, ranked hypotheses, falsification results, and ledger (see Debug Mantra below). Produce a concise **evidence map** (`templates.md`). If exploration invalidates a confirmed decision, return to Phase 1 for that branch only, ask one Socratic question, reconfirm the amended summary.

## Debug Mantra Protocol (apply only when the outcome includes fixing faulty behavior)

As the first user-visible content of the first debugging response, recite verbatim once:

> **Mantra:**
> 1. **First is reproducibility.** Can the issue be reproduced reliably?
> 2. **Know the fail path.** Debugger first; then source trace + knob enumeration; then in-code instrumentation.
> 3. **Question your hypothesis.** What would disprove it?
> 4. **Every run is a breadcrumb.** Cross-reference all of them.

If told to skip the mantra, skip only the recital and still apply the protocol. Before proposing/implementing a fix:
1. **Reproduce** — a fast deterministic pass/fail signal captured as a failing test/runnable artifact. If flaky, raise the reproduction rate first. No reproduction ⇒ stop and report missing evidence. (This reproduction becomes the loop's first seed example.)
2. **Trace the fail path** — debugger preferred; else source trace + enumerate every config/input/branch/timing/concurrency/build knob; tagged in-code instrumentation only after those are exhausted.
3. **Falsify** — keep 3–5 ranked hypotheses; for the leader state the simplest proof and cleanest disproof, run the disproof first.
4. **Cross-reference** — an experiment ledger of each run: what changed, what happened, what it ruled in/out. Reject any hypothesis contradicted by an earlier breadcrumb.

No fix before reproduction + fail-path evidence. No accepted root cause until its hypothesis survives falsification and explains every ledger entry. Remove temporary instrumentation before completion.
