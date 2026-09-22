---
name: cog3
description: "Provider-agnostic Socratic spec-to-code workflow that fuses Cog2's discipline with the autoresearch (property-as-metric) loop. It resolves the plan by questioning, explores the codebase, decomposes work into atomic two-file handoffs — each carrying a property-based acceptance metric plus seed examples — then drives every handoff to green with an autonomous generate-test-score-mutate loop that freezes each property counterexample into a permanent example test, stabilizes across seeds, and refactors; finally it simplifies touched code and reviews the touched architecture. Enforces goal mode and, for bugs, the Debug Mantra. Use when the user mentions cog3, or wants a plan or bug turned into property-tested code with assumptions surfaced first, atomic handoffs, autonomous test-driven implementation, and a final architecture review."
---

# Cog3

Cog3 = **Cog2's Socratic decomposition front-end + the autoresearch loop as the per-handoff execution engine.** Cog2 decides *what* to build (questioned, decomposed into atomic behaviors, human-gated) and verifies each behavior with example-based TDD. Cog3 adds the **property layer**: each atomic handoff defines universally-quantified properties as its acceptance metric, then is driven to correct *autonomously* by a generate-test-score-mutate loop where the Gherkin scenario seeds the example suite and every counterexample is frozen into a permanent regression. Cog2 = scope + human gates; the loop = drive each piece to provably-correct.

**Execute the workflow; do not merely describe it.** Move through five strict phases — read each phase's reference when you reach it (keeps context small):

1. **Socratic convergence** + **2. Codebase exploration** (+ Debug Mantra for bugs) — `references/socratic.md`
3. **Gherkin + atomic decomposition** with a property metric per handoff — `references/decomposition.md`
4. **Autoresearch execution** (the per-handoff loop) — `references/execution.md`, engine in `references/autoresearch-loop.md`
5. **Simplification + architecture review** and **Completion** — `references/execution.md`

Templates for every gate/handoff/report: `references/templates.md`. Domain property kinds: `references/ui-mode.md`, `references/domains.md`.

## Goal mode

Invoking Cog3 enters goal mode. Initialize tracking with the first available adapter: (1) native goal API; (2) persistent task/plan API; (3) portable **Goal Ledger** (`references/templates.md`), persisted at `docs/cog3/<plan-slug>/goal.md` after repo editing is approved. Keep the goal active through both human gates and all handoffs; mark complete only after every completion condition is verified. Treat provider features as **adapters, not workflow semantics** — discover capabilities, map them, fall back to conversational/file-based when absent. If a provider can't read/write/execute the repo, finish the interview + handoff plan, keep the goal active, and report the missing execution capability as the blocker.

## Non-negotiables (full detail in the phase references)

- **Three human gates:** confirm the shared-understanding summary before inspecting/editing the repo; approve the Gherkin + handoff plan before any production code; observe a real **Red** failure before writing code. Never self-approve.
- **One question per turn** in the interview; surface assumptions; never invent requirements (park them with risk).
- Every task = **two handoff files** (`.feature` + `.md`) **+ its property metric + seed examples**; recursively split to **one observable behavioral delta** with an **Atomicity Proof**.
- Execution discipline per handoff = the **autoresearch loop** (`references/autoresearch-loop.md`): properties are the metric; mutate-from-best; **no git commits** (working-tree keep/discard via `.loop/best/`, kept only when green tests say so); freeze every counterexample; pin the PBT seed; stabilize across 3 seeds. Per handoff, **plan & review the skeleton once before the loop**: with the plan approved (Phase 3), insert the spec-tagged TODOs into the code → **request a review** (the checkpoint before the autonomous loop); then the **loop only fills** them spec-driven (markers removed on green, none left behind) — one candidate per cycle, no per-cycle review.
- **Stay scoped** to the handoff's surface; preserve unrelated changes. After all green, **simplify only Cog3-touched code**, then a **bounded architecture review** (report deepening opportunities, don't implement unapproved expansion).
- For bugs, the **Debug Mantra** is mandatory (`references/socratic.md`).
- **Orchestrate via workflows** (adapter — use the provider's workflow / parallel sub-agent capability, else inline): **exploration** = parallel codebase mappers → synthesized evidence map; **independent handoffs** (no dependency between them) run as **parallel workflows**; the TODO-skeleton **code-review** and per-cycle candidate generation fan out by lens/angle; delegate substantial actions. Per-handoff **dependency order** and the loop's **keep/discard ordering** are always preserved; human gates never run in parallel.
