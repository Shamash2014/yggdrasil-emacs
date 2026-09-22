# Phases 4–6 — Red Baseline, the Loop, Stabilize/Refactor

## Phase 4 — Red Baseline + Loop Setup

### Set up state tracking

Create `.autoresearch/` in the repo root:

1. **`properties.md`** — the agreed properties (archetype + generator notes). The frozen acceptance contract.
2. **`examples.md`** — running log of example tests, each with input, expected output, and origin (`seed` | `counterexample@run<N>` | `regression`).
   - **`plan.md`** — the materialized TODO skeleton (the mirror of every in-code `TODO(...)`, each → its property/example). This is the durable plan and the "all filled?" audit list, since the in-code markers are transient and removed as they're filled.
3. **`best_results.json`** — the per-test result vector of the current best implementation, over the *current* suite (makes regression detection computable without re-running best):
   ```json
   { "properties": {"P1": true, "P2": false}, "examples": {"E1": true, "E2": false} }
   ```
4. **`state.json`**:
   ```json
   {
     "goal": "[goal]", "surface": "[function/module]",
     "test_cmd": "[exact command]", "reporter": "[per-test output flag]",
     "pbt_seed": 1234, "property_count": 0, "example_count": 0,
     "best_pass_count": -1, "best_all_green": false, "green_streak": 0,
     "phase": "correctness", "run_number": 0, "plateau_counter": 0,
     "max_cycles": 30
   }
   ```
   `pbt_seed` is fixed so in-loop runs are reproducible. `max_cycles` is the autonomy budget (raise if the user asks). `phase` moves `correctness → stabilize → refactor`.
5. **`results.jsonl`** — empty; appended each cycle.

Add `.autoresearch/` to `.gitignore` if one exists and the entry is absent. **No git commits** — the loop runs entirely in the working tree. Define the **scope** = the target surface plus its tests (the only files the run may edit). Keep `.autoresearch/best/` as a **complete mirror of the scope** at the best accepted state — the no-git revert baseline. **KEEP**: mirror the current scope into `best/` with a delete-syncing copy (e.g. `rsync -a --delete <scope> .autoresearch/best/`). **DISCARD**: mirror `best/` back over the scope the same way. Because it's a *whole-subtree* mirror, file creations, deletions, and renames revert automatically — **no per-file change tracking**. The final result is left as uncommitted working-tree changes for the user to review and commit.

### Write the tests first

Write the property tests and seed example tests **before any implementation** — they define the metric and stay red until the loop fills the skeleton. (The red baseline is recorded in Phase 4b, once the skeleton compiles, so the failure is *behavioral* — not a missing-symbol error.)

## Phase 4b — Plan & review the skeleton, then take the red baseline (once, before the loop)

With the tests written and the plan settled, build the **TODO-annotated code skeleton once, outside the loop** — this is the planning; the loop that follows only *fills* it.

1. **Structures + connections.** Define/extend the structs/types the behavior needs **and the connections among them** — ownership, references, dependencies, data-flow edges, wiring — before any logic.
2. **Interfaces.** Declare the function/class signatures (the contract surface) as stubs (`todo!()` / `raise NotImplementedError` / `throw`) so the Red tests fail on *behavior*, not a compile/import error.
3. **TODO change-sites.** Write a tracked `TODO(goal → P2/E3)` **comment into the code** at **every** change site the plan implies — co-located with the stub it belongs to (e.g. inside the body of each stubbed function), tagged with the property it feeds or the example it satisfies. The **in-code TODOs are the review artifact** (the reviewer reads them in place); `.autoresearch/plan.md` is only an audit mirror of the same list. This step is real file edits — do not skip it and jump to filling; without the markers there is nothing to review. They are transient scaffolding (removed as each is filled — none left behind).
4. **Break + revert points.** Mark where the change must break existing code (a shared signature, schema, or caller); land the smallest break the tests need. A DISCARD restores the whole scope from `.autoresearch/best/`, so breaks revert cleanly.
5. **Invariants + defensive checks.** Add internal assertions — preconditions, postconditions, structural invariants — in the code; they complement the external property tests and document intent, never replacing a property.

Now **run the suite — it MUST fail on *behavior*** (the skeleton compiles, so a failure means missing logic, not a missing symbol; if a property passes against the all-stub skeleton it is too weak — strengthen it). This is the **red baseline, run 0**: write `best_results.json` and set `best_pass_count` to the stub's example-pass count (usually 0). **Verify the skeleton is materialized before reviewing:** grep the scope for `TODO(` — **every** planned change site must carry its marker in the code, and the count must match `plan.md`. An empty or short result means the TODOs were never written; insert them now — there is nothing to review otherwise. Then **request a review** of the in-code skeleton — a reviewer/code-review adapter or review workflow, or the user — the **checkpoint before the autonomous loop** (design fixes are cheap at skeleton stage). **Apply the review's fixes** to the skeleton (cheap at this stage) and re-verify the TODO grep; then mirror the reviewed skeleton into `.autoresearch/best/`; the tree is now best — invariant: **at the start of every cycle the working tree equals best (matches `.autoresearch/best/`).** Begin the loop.

```
Red baseline (run 0): 0/[P] properties green · [k]/[E] examples passing
Working tree, no commits · Test cmd: [cmd] · Seed: [pbt_seed] · Budget: [max_cycles] cycles
```

During the loop the skeleton changes only in two bounded ways — a frozen counterexample adds an example *test* (step 6) that a later cycle's operator satisfies by editing code (**no new reviewed TODO, no re-review**), and **plateau-break** rewrites the skeleton from scratch (re-review). There is **no per-cycle skeleton review.**

## Phase 5 — The Loop (correctness phase)

Run autonomously, without asking permission between cycles. **Disk is the source of truth** — re-read `state.json`, `properties.md`, `examples.md`, `best_results.json`, and the last 5 lines of `results.jsonl` at the start of every cycle.

**Cycle invariant:** at step 1 the working tree equals best (mirrored in `.autoresearch/best/`), and `best_results.json` is its per-test vector over the **current** suite `S`. Scoring compares the mutated implementation against `best_results` on the *same* `S` — apples to apples. New counterexamples are frozen **after** the comparison, never during it, so the denominator never moves mid-comparison.

### One outer cycle

#### 1. Load state from disk
Read `state.json`, `examples.md`, `best_results.json`, last 5 `results.jsonl`. Confirm the working tree equals best (matches `.autoresearch/best/`).

#### 2. Select an operator and fill (mutate from best)
Operator selection is **state-dependent, not round-robin** — pick what the current failures call for, and log it:
- examples failing → **pass-the-simplest-failure** or **generalize-from-counterexample**
- examples pass but a property still fails on a region → **strengthen-then-satisfy**
- generators keep hitting a boundary → **edge-hardening** (empty, null, zero, max, overflow, unicode, concurrency)
- **plateau-break override:** if `plateau_counter >= 5`, ignore the above and select **plateau-break** (step 9) — re-plan the skeleton from scratch (with re-review) instead of mutating from best. (refactor-green is Phase 6 only.)

Mutate **from best** by **filling the Phase-4b skeleton's TODOs that the operator targets** (a subset — the rest may stay red; that's expected), each driven by its tagged spec: watch it fail, fill the body until it passes. An **example**-tagged TODO closes when its example greens; a **property** is an *aggregate gate* (greens only once all its contributing TODOs are filled — a property-tagged TODO is *planned-done* when its code is filled, even if the property stays red pending siblings). **Implementation relies on green tests** — never treat a TODO complete on filled-but-red code; the candidate becomes best only when the outer score (step 4) is green/gain (no commit). **When a TODO's spec greens, delete its marker** (mark it done in `plan.md`) — no `TODO(...)` left behind.

If filling reveals a missing or wrong skeleton element, do a **small local re-plan** (add or fix that one TODO) — the full skeleton and its review already happened in Phase 4b; only plateau-break re-plans wholesale. **Orchestrate via a workflow where available:** generate several candidate fills in parallel and carry the best forward (parallelism is *within* this step; only the chosen candidate goes to step 3, preserving keep/discard ordering). Hand the candidate to outer step 3.

#### 3. Run the tests (evaluate)
Run the test command with the pinned `pbt_seed` and the per-test reporter. Parse a **per-test result map** `V_cur` = `{properties, examples}`. Exit code alone is insufficient. For each failed property, capture the **shrunk counterexample** (reproducible because the seed is pinned).

#### 4. Score and compare (on the current suite `S`, against `best_results`)
```
no_regress       = every property/example that best_results marks true is also true in V_cur
gain             = (count of true in V_cur) > best_pass_count          # same suite S
all_green        = every property AND every example in V_cur is true   # the FULL suite
first_full_green = all_green AND not best_all_green                    # best_all_green ≡ best is fully green
KEEP if  first_full_green OR (gain AND no_regress)  else  DISCARD
# Note: transition to stabilize requires the full suite green, never properties-only — so no red
# example (regressed or orthogonal-and-never-passed) can sneak into stabilize. all_green subsumes
# no-example-regress. An example the properties don't imply just keeps the loop in correctness
# (mutating to satisfy it) until it greens or the budget stops it — never a stuck stabilize.
```

#### 5. Apply keep/discard
```
KEEP (green tests say so — no commit):
         mirror the scope into .autoresearch/best/ (delete-syncing copy); best_results=V_cur; best_all_green = all_green (full suite); plateau_counter=0
DISCARD: mirror .autoresearch/best/ back over the scope (undo this candidate); plateau_counter += 1
```

#### 6. Freeze counterexamples (AFTER the comparison)
For each property that failed in `V_cur`, add its shrunk counterexample as a new example test (named, tagged `counterexample@run<N>`). Suite grows `S → S'`. Run **only the newly added tests** against the current tree (== best) and append pass/fail to `best_results.examples`; bump `example_count`. Then **recompute `best_pass_count` = count of `true` in `best_results`** over `S'` (always derived, never stale).

#### 7. Log + update state
Append one JSON line to `.autoresearch/results.jsonl`:
```json
{ "run": 1, "timestamp": "ISO 8601", "phase": "correctness",
  "properties_green": 0, "property_total": 0, "examples_passing": 0, "example_total": 0,
  "status": "keep | discard",
  "mutation_operator": "pass_simplest_failure | generalize_counterexample | strengthen_then_satisfy | edge_hardening | plateau_break | refactor_green",
  "new_counterexamples": ["shrunk input frozen into an example test"],
  "failures": ["brief: which property/example failed and why"], "files": ["files touched this candidate"] }
```
Update `state.json` (`run_number`, `best_pass_count`, `best_all_green`, `plateau_counter`, `example_count`).

#### 8. Report + transition
`RUN [n] | Props: [g]/[P] | Examples: [k]/[E] | Status: [KEEP/DISCARD] | Best pass: [bp]` + mutation + new counterexamples + top failures. If `best_all_green` just became true **and no `TODO(...)` marker remains anywhere in the scope** (grep is clean) → `phase = "stabilize"` and go to Phase 6. A leftover TODO is unfilled work even if tests pass — stay in correctness and fill it first.

#### 9. Plateau breaker (the operator step 2 selects when `plateau_counter >= 5`)
This is **not** a post-apply action — it's the operator step 2 chooses when the counter is maxed, so it flows through run/score/apply (steps 3–5) like any candidate. When triggered: **don't mutate from best** — re-read the last 10 `results.jsonl` entries and **re-plan the skeleton from scratch** (re-run Phase 4b, including its review) using only the goal, properties, and accumulated counterexamples; ignore the stuck structure. Log `mutation_operator: plateau_break`. **After step 5's apply, reset `plateau_counter = 0`** (overriding the +1 a DISCARD sets) — a restart with memory, kept or discarded by the normal step-4 rule.

#### 10. Budget + continue
Every 10 cycles, print a checkpoint (best pass count, properties green, top failures) without pausing. If `run_number >= max_cycles`, stop with a "budget reached" report. Otherwise go to step 1.

## Phase 6 — Stabilize, then Refactor (once the full suite is green)

### Stabilize (`phase: "stabilize"`)
Do **not** mutate. Re-run the **full suite** (properties + examples) against best with a **new, different PBT seed** each run. **Green = the entire suite passes.**
- Fully green again → `green_streak += 1`.
- A run surfaces a property **counterexample**, *or* a previously-green **example** goes red → it's a real residual regression: freeze the counterexample (if any), set `best_all_green=false`, `green_streak=0`, `phase="correctness"`, return to Phase 5 to fix it.

When `green_streak` reaches **3** (three distinct seeds, all fully green) → set `phase = "refactor"`.

### Refactor (`phase: "refactor"`, bounded — default ≤ 3 cycles)
Apply **refactor-green** (simplify, no behavior change). Refactor runs the normal cycle (steps 1–8 + budget) with the step-4 score replaced by the rule below; there are no failures to freeze. Because correctness is maxed (gain impossible), use a different KEEP rule:
```
KEEP if  all properties green AND all examples pass (pinned seed) AND complexity not worse (LOC, or cyclomatic if a tool exists)
else DISCARD (mirror .autoresearch/best/ back over the scope)
```
After the refactor budget is spent (or two consecutive DISCARDs), **stop**.

### Final report
```
AUTORESEARCH COMPLETE
  Goal: [goal]   ·   Runs: [total]   ·   Phase reached: refactor
  Properties: [P]/[P] green, stable across [green_streak] seeds
  Examples:   [E]/[E] passing  ([E_start] → [E] grown from counterexamples)
  TODOs: 0 remaining in scope (all filled; markers removed)
  Counterexamples frozen into regressions: [count]
  Most effective operators: [ranked by KEEP rate]
Implementation: [files changed]  ·  Tests: [test files]  ·  Result: uncommitted working-tree changes (no commits)  ·  History: .autoresearch/results.jsonl
```

## Operational Rules

1. **Properties are the metric.** All properties green, stable across 3 distinct seeds, is the only definition of done.
2. **Examples are the gradient.** Every property counterexample is frozen into a permanent example test. The suite only grows.
3. **Never weaken the contract to pass.** Don't loosen a property, delete an example, or special-case to a known test input. Strengthen properties; never weaken them.
4. **Tests first, red before green.** Confirm the tests fail for the intended reason first. A property that can't fail is rejected as too weak.
5. **Score on a fixed suite, freeze after.** Compare against `best_results` on the same `S`; freeze counterexamples only after deciding keep/discard. Never compare across a changed denominator.
6. **Mutate from best**, never from a discarded attempt. Invariant: the working tree equals best (the `.autoresearch/best/` snapshot) at step 1.
7. **Persist per-test results.** `best_results.json` holds best's vector; extend it (run only new tests) when the suite grows. `best_pass_count` is always the derived count of `true` in `best_results` — recompute after every growth.
8. **Re-read state from disk every cycle.** Conversational memory is not trusted for state, scores, or counterexamples.
9. **No git commits.** The loop runs in the working tree; KEEP mirrors the green change into `.autoresearch/best/`, DISCARD restores from it. **Implementation relies on green tests** — a change is "kept" only when the outer per-test score is green/gain, never via a commit. Leave the final result as uncommitted changes for the user to review/commit.
10. **Pin the PBT seed in the loop; vary it only to stabilize.** A property failing on inputs an earlier run didn't generate is the generator working — freeze the counterexample, don't call it flaky. True flakiness (same pinned input, different result) is red until stabilized.
11. **Need per-test output.** Use a JSON/JUnit/TAP reporter (or run tests by name); exit code can't tell you how many properties/examples passed.
12. **Refactor only in Phase 6**, with the complexity-not-worse rule — never in the correctness phase.
13. **Respect the budget.** Every run — correctness, stabilize, or refactor — increments `run_number` and counts toward `max_cycles`; stop at `max_cycles`. Checkpoint every 10 runs without pausing.
14. **Stay scoped.** Modify only the target surface and its tests.
15. **Log everything** — every cycle gets a JSONL entry with counterexamples and the operator used.
16. **Plan the skeleton once, outside the loop (Phase 4b), and review it before the loop starts** — structures+connections → stubbed interfaces → TODO change-sites (tagged with the property they feed / example they satisfy) → break/revert points → invariants, all inserted in code as the **code-review artifact**. The loop then only *fills* those TODOs (example-tagged close per-TODO; a property is an aggregate gate greening once all its TODOs are filled). **No per-cycle skeleton review** — only plateau-break re-plans (and re-reviews) wholesale; a frozen counterexample adds an example *test* the operator later satisfies by editing code (no new reviewed TODO). Internal invariants complement, never replace, the property tests.
17. **No TODO left behind.** The `TODO(...)` markers are transient scaffolding (mirrored in `plan.md`); each is deleted when its spec greens. The loop cannot enter stabilize, and the goal cannot complete, while any `TODO(...)` remains in the scope — a leftover marker is unfilled work even if the suite is green. Honors the no-stray-comments rule.
18. **Specialized domains:** read `ui-mode.md` (frontend) or `domains.md` (data/ML, external I/O, concurrency, performance, generative, numerical) before writing properties. The golden-approval checkpoint in UI mode is the *only* sanctioned pause to autonomy; any llm-judge is advisory and never gates KEEP/DISCARD.
