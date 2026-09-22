# Per-handoff autoresearch loop (Cog3 Phase 4 engine)

This replaces a plain single red-green-refactor cycle with an autonomous **property-as-metric** loop, run **once per approved handoff**. The handoff's properties are the acceptance metric; its Gherkin scenario(s) seed the example suite; every property counterexample is frozen into a permanent example test. The implementation is what gets optimized; the test suites are the fitness function.

Scope discipline is inherited from Cog2: the loop touches only the handoff's target surface and its tests, never pulls in other handoffs' scenarios, and stops to re-approve if it discovers a new behavioral delta (atomicity violation).

## State (per handoff, under `docs/cog3/<plan-slug>/tasks/<NN>-<slug>/.loop/`)

- `best_results.json` — best impl's per-test vector over the **current** suite: `{ "properties": {...bool}, "examples": {...bool} }`.
- `state.json`:
  ```json
  {
    "handoff": "<NN>-<slug>", "surface": "...", "test_cmd": "...", "reporter": "...",
    "pbt_seed": 1234, "property_count": 0, "example_count": 0,
    "best_pass_count": -1, "best_all_green": false, "green_streak": 0,
    "phase": "correctness", "run_number": 0, "plateau_counter": 0,
    "max_cycles": 20
  }
  ```
- `plan.md` — the materialized TODO skeleton (audit mirror of every in-code `TODO(...)`, each → its property/example); the "all filled?" list.
- `results.jsonl` — one line per cycle.

`pbt_seed` is pinned so in-loop runs are reproducible; vary it only in Stabilize. `max_cycles` is the per-handoff budget. Disk is the source of truth — re-read every cycle.

## Write the tests + scope

Write the handoff's property tests **and** its Gherkin scenario(s) as seed example tests **before** any implementation — they stay red until the loop fills the skeleton. Define the **scope** = the handoff's surface plus its tests (the only files this handoff may edit). **No git commits** — the loop runs in the working tree; `.loop/best/` is a **complete mirror of the scope** and the revert baseline. **KEEP**: mirror the scope into `.loop/best/` (delete-syncing copy, e.g. `rsync -a --delete`); **DISCARD**: mirror `.loop/best/` back over the scope. Whole-subtree mirroring reverts creations, deletions, and renames automatically — **no per-file tracking**. **Cycle invariant: at the start of every cycle the working tree equals best (matches `.loop/best/`).**

## Plan & review the skeleton, then take the red baseline (once, before the loop)

The handoff plan is approved (Phase 3 gate). Before the loop, build the **TODO-annotated code skeleton once** within the handoff scope:
1. **Structures + connections** — structs/types and the connections among them (ownership, references, dependencies, data-flow edges) before logic.
2. **Interfaces** — stub the signatures (`todo!()` / `raise NotImplementedError` / `throw`) so Red fails on *behavior*, not compile.
3. **TODO change-sites** — write a tracked `TODO(<NN>-<slug> → P2/E3)` **comment into the code** at **every** change site the plan implies, co-located with its stub, tagged with the property it feeds / example it satisfies. The **in-code TODOs are the review artifact**; `.loop/plan.md` holds an audit mirror of the same list. These are real file edits — do not skip to filling; without the markers there is nothing to review. Transient scaffolding — removed as each is filled (none left behind).
4. **Break + revert points** — mark where the change must break existing code; smallest break the tests need; a DISCARD restores the whole scope from `.loop/best/`. This also forces a Cog2 atomicity recheck — if the break spans another behavioral delta, stop and split the handoff before proceeding.
5. **Invariants + defensive checks** — internal pre/postconditions/structural asserts that complement the property tests.

Then **run the suite — it MUST fail on *behavior*** (the skeleton compiles; strengthen any property that passes against the all-stub skeleton, surfacing it to the Cog2 handoff record). This is the **red baseline, run 0**: record `best_results.json`. **Verify the skeleton is materialized before reviewing:** grep the scope for `TODO(` — **every** planned change site must carry its marker in the code (count matches `.loop/plan.md`); if empty or short, insert the missing TODOs now (nothing to review otherwise). **Request a review** of the in-code skeleton — a reviewer/code-review adapter or review workflow, or the user — a **per-handoff checkpoint *before* the autonomous loop** (design fixes are cheap at skeleton stage); it is **not** a mid-loop stop. **Apply the review's fixes** to the skeleton and re-verify the TODO grep, then mirror the reviewed skeleton into `.loop/best/` and begin the loop. During the loop the skeleton changes only via a frozen counterexample — which adds an example *test* (step 6) that a later cycle's operator satisfies by editing code, **no new reviewed TODO and no re-review** — or plateau-break (wholesale re-plan + re-review). **No per-cycle review.**

## One cycle (correctness phase)

1. **Load** `state.json`, `best_results.json`, last 5 `results.jsonl`. Confirm the working tree equals best (matches `.loop/best/`).
2. **Select operator + fill** (state-dependent, not round-robin), mutating from best:
   - examples failing → *pass-the-simplest-failure* or *generalize-from-counterexample*
   - examples pass, a property still fails on a region → *strengthen-then-satisfy*
   - generators keep hitting a boundary → *edge-hardening*
   - **plateau-break override:** if `plateau_counter >= 5`, ignore the above and select *plateau-break* (step 9) — re-plan the skeleton from scratch (with re-review) instead of mutating from best.

   Mutate from best by **filling the pre-built skeleton's TODOs the operator targets** (a subset; the rest may stay red), each driven by its tagged spec (watch it fail, fill until it passes). An **example**-tagged TODO closes when its example greens; a **property** is an *aggregate gate* (greens only once all its contributing TODOs are filled — a property-tagged TODO is *planned-done* when its code is filled, even if the property stays red pending siblings). **Implementation relies on green tests** — never treat a TODO complete on filled-but-red code; the candidate becomes best only when the outer score (step 4) is green/gain (no commit). **When a TODO's spec greens, delete its marker** (mark it done in `.loop/plan.md`) — no `TODO(...)` left behind. If filling reveals a missing/wrong skeleton element, do a **small local re-plan** (that one TODO); only plateau-break re-plans wholesale. **Workflow where available:** generate several candidate fills in parallel and carry the best forward (only the chosen candidate goes to step 3). Hand the candidate to the outer run/score.
3. **Run** with the pinned `pbt_seed` and a per-test reporter (JSON/JUnit/TAP). Parse `V_cur = {properties, examples}` (which pass). Capture each failed property's **shrunk counterexample**.
4. **Score on the current suite `S` vs `best_results`:**
   ```
   no_regress       = every property/example true in best_results is also true in V_cur
   gain             = count(true in V_cur) > best_pass_count        # same suite S
   all_green        = every property AND every example in V_cur is true   # the FULL suite
   first_full_green = all_green AND not best_all_green              # best_all_green ≡ best is fully green
   KEEP if first_full_green OR (gain AND no_regress) else DISCARD
   # Transition to stabilize requires the FULL suite green, never properties-only — so no red example
   # (regressed, or orthogonal-and-never-passed, e.g. the Gherkin scenario) can sneak into stabilize.
   # all_green subsumes no-example-regress; an example the properties don't imply keeps the loop in
   # correctness until it greens or the budget stops it — never a stuck stabilize.
   ```
5. **Apply:**
   - KEEP (green tests say so — no commit) → mirror the scope into `.loop/best/` (delete-syncing copy); `best_results=V_cur`; `best_all_green = all_green (full suite)`; `plateau_counter=0`.
   - DISCARD → mirror `.loop/best/` back over the scope (undo this candidate); `plateau_counter += 1`.
6. **Freeze counterexamples (AFTER step 4):** for each failed property, add its shrunk counterexample as a named example test (tagged `counterexample@run<N>`), suite `S→S'`. Run **only the new tests** against the current tree (== best) and append to `best_results.examples`; bump `example_count`. Then **recompute `best_pass_count` = count of `true` in `best_results`** over `S'` (always derived, never stale), so next cycle's `gain` compares on the same denominator.
7. **Log + update state** (`run_number`, `best_pass_count`, `best_all_green`, `plateau_counter`, `example_count`).
8. **Report** one line. If `best_all_green` just became true **and no `TODO(...)` marker remains in the handoff scope** (grep clean) → `phase = "stabilize"`. A leftover TODO is unfilled work even if tests pass — stay in correctness and fill it first.
9. **Plateau breaker** (the operator step 2 selects when `plateau_counter >= 5`) — not a post-apply action; it flows through run/score/apply (steps 3–5) like any candidate. When triggered: don't mutate from best; **re-plan the skeleton from scratch** (re-run the pre-loop planning + review) using only the handoff outcome, properties, and accumulated counterexamples (last 10 `results.jsonl`). Log `plateau_break`. **After step 5's apply, reset `plateau_counter = 0`** (overriding the +1 a DISCARD sets) — kept or discarded by the normal step-4 rule.
10. **Budget:** checkpoint every 10 cycles without pausing; if `run_number >= max_cycles`, stop the handoff with a "budget reached" status and report it to the Cog2 goal ledger (do not silently mark complete).

## Stabilize → Refactor (once the full suite is green)

- **Stabilize** (`phase: stabilize`): no mutation; re-run best (**full suite**) with a **new seed** each run. **Green = the entire suite (properties + examples) passes.** Fully green → `green_streak += 1`. A new property **counterexample** *or* a previously-green **example** going red → it's a real residual regression: freeze the counterexample (if any), set `best_all_green=false`, `green_streak=0`, `phase=correctness`, return to the correctness loop to fix it (Cog3 Phase 4's loop — *not* the cross-handoff Phase 5 simplification). At `green_streak == 3` → `phase: refactor`.
- **Refactor** (`phase: refactor`, ≤ 3 cycles): apply refactor-green (simplify, no behavior change) — runs the normal cycle with the step-4 score replaced by this rule (nothing to freeze). KEEP iff all properties green AND all examples pass (pinned seed) AND complexity not worse (LOC / cyclomatic); else DISCARD. Then the handoff is done.

## Handoff completion → back to Cog2

A handoff is complete only when: all its properties are green and stable across 3 seeds, every seed/regression example passes, the red baseline showed a genuine failure first, the refactor pass left the suite green, and **no `TODO(...)` marker remains in the handoff scope** (all filled, markers removed). Record into the handoff `.md`: red evidence, property+example pass vectors, counterexamples frozen (count + inputs), seeds used, files changed, and any budget/atomicity deviation. Then advance the Cog2 goal ledger to the next handoff. **All handoff code lives uncommitted in the working tree (no git commits accumulate across handoffs).** If the user wants per-handoff checkpoints, surface that they can commit the completed handoff before the next one begins.

## Loop invariants (don't skip)

- **Re-read state from disk every cycle** — `state.json`, `best_results.json`, last 5 `results.jsonl`; never trust conversational memory for scores/counterexamples.
- **Per-test output required** — a JSON/JUnit/TAP reporter (or run tests by name); exit code alone can't tell you which properties/examples passed.
- **Score on the fixed suite, freeze after** — compare against `best_results` on the same `S`; freeze counterexamples only after the keep/discard decision. `best_pass_count` is always the derived count of `true` in `best_results`, recomputed after every suite growth.
- **Mutate from best; no git commits** — the tree equals best (`.loop/best/`) at step 1; KEEP mirrors the green change into `.loop/best/`, DISCARD restores from it; a change is kept only when green tests say so.
- **Budget every run** — correctness, stabilize, and refactor runs each increment `run_number` and count toward `max_cycles`; stop at the budget.
- **No TODO left behind** — `TODO(...)` markers are transient scaffolding (mirrored in `.loop/plan.md`), deleted as each spec greens. No entry to stabilize and no handoff completion while any `TODO(...)` remains in scope, even if the suite is green. Honors the no-stray-comments rule.
- **Pin the PBT seed in-loop; vary only in stabilize.** A property failing on inputs an earlier run didn't generate is the generator working — freeze it; true flakiness (same pinned input, different result) is red until stabilized.

## Anti-gaming (inherited, absolute)

Never weaken a property, delete an example, or special-case the implementation to a known test input to manufacture a gain. Properties may only be *strengthened*. A property failing on inputs an earlier run didn't generate is the generator working — freeze the counterexample; that is not flakiness. True flakiness (same pinned input, different result) is red until stabilized.
