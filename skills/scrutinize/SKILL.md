---
name: scrutinize
description: Outsider-perspective end-to-end review of a plan, PR, or code change, then simplification driven by what the review found. First questions intent and whether a simpler/more elegant approach would achieve the same goal, traces the actual code path (not just the diff) to verify the change does what it claims, reports findings — then applies the simplifications those findings justify, preserving behavior exactly. Trigger on /scrutinize and proactively whenever the user asks to review, audit, sanity-check, or get a second opinion on a plan, PR, diff, design doc, or proposed code change, or asks to simplify, clean up, or refine recently written code.
model: opus
---

# Scrutinize

Stand outside the change and ask whether it should exist at all, verify it actually does what it claims end-to-end, then simplify what the review proved worth simplifying.

## Operating stance

- **Outsider.** Forget who wrote it and why they think it's right. Read the artifact cold.
- **End-to-end, not diff-local.** The diff is the entry point, not the scope. Follow the call graph through real code paths.
- **Actionable, concise, with rationale.** Every finding states *what to change*, *why*, and *what evidence* led you there. No filler, no restating the diff back.
- **Review earns the edit.** Steps 1–4 produce findings; step 5 only applies what those findings support. Never simplify code you have not traced.

## Workflow

Run these in order. Do not skip ahead.

### 1. Intent — what is this actually trying to do?

- State the goal in one sentence, in your own words. If you cannot, the artifact is underspecified — say so and stop.
- Ask: **is there a simpler, smaller, or more elegant way to achieve the same goal?** Consider:
  - Doing nothing (is the problem real / load-bearing?).
  - Using something that already exists in the codebase instead of adding new surface.
  - A smaller change that solves 90% of the goal with 10% of the risk.
  - Solving it at a different layer (config vs code, framework vs app, build vs runtime).
- If a better alternative exists, name it explicitly with rationale. This is the most valuable thing you can output — surface it before the line-by-line review.

### 2. Trace — walk the actual code path

- For each behavior the change claims, trace the path end-to-end through the real code, not just the lines in the diff:
  - Entry point → call sites → branches taken → state mutated → exit / return / side effect.
  - Include the unchanged code on either side of the diff. Bugs hide at the seams.
- For a plan or design doc: trace the proposed flow against the existing system. Where does it touch reality? What does it assume that isn't true?
- Note every place the trace surprises you (unexpected branch, dead code reached, state you didn't know existed). Surprises are signal.

### 3. Verify — does it actually do what it claims?

For each claim the change/plan makes, answer:

- **Does the code path you just traced actually produce that behavior?** Walk it explicitly. "It claims X. Path: A → B → C. At C, [observation]. Therefore [holds / doesn't hold]."
- **What inputs / states would break it?** Edge cases, concurrent callers, error paths, partial failures, retries, empty/null/unicode/huge inputs, ordering assumptions.
- **What does it silently change?** Performance, error semantics, observability, contract for other callers, on-disk / on-wire format.
- **How is it tested?** Do the tests actually exercise the traced path, or do they pass while skipping it (mocks that hide the bug, asserts on intermediate state, happy path only)?

### 4. Report

Output one tight section per finding. Order by severity (blocker → major → nit). For each:

- **Finding** — one sentence, specific. Cite `file:line` when applicable.
- **Why it matters** — the consequence, not the principle.
- **Evidence** — the trace step or input that exposes it.
- **Suggested change** — concrete, minimal.

Close with a one-line verdict: ship / fix-then-ship / rework / reject — with the single biggest reason.

### 5. Simplify — apply what the review justified

Act on the findings, in severity order. Scope is the code the review traced — recently modified code unless told otherwise.

- **Blockers and correctness findings first.** A simplification pass over broken logic is wasted work. If step 4's verdict is rework or reject, stop here and say why: the right move is a different change, not a cleaner version of this one.
- **Preserve functionality exactly.** Only how the code does it changes, never what it does. Outputs, side effects, error semantics, and the contract for other callers stay intact.
- **Apply the project's standards**, read from CLAUDE.md and the surrounding code rather than assumed: module and import conventions, function vs arrow style, return type annotations, component and props patterns, error handling, naming.
- **Reduce real complexity**: collapse unnecessary nesting, delete redundant code and single-use abstractions, consolidate logic that belongs together, rename for clarity, drop comments that restate the code. Prefer switch or if/else chains over nested ternaries.
- **Stop at the point of diminishing clarity.** Explicit beats compact. Leave the helpful abstraction, the separate concern, and the debuggable shape alone — fewer lines is not the goal.
- Re-trace each edited path after editing, the same way step 2 did, and say what you re-traced. Report only the changes that affect understanding; skip the inventory of cosmetic edits.

## Operating rules

- **No rubber-stamps.** "LGTM" is not an output. If you genuinely find nothing, say what you traced and what you checked, so the user can judge whether your review covered the surface they cared about.
- **Cite or it didn't happen.** Every claim about the code references a specific path, file, or line. No vague "this might break under load."
- **Distinguish claim from verification.** "The PR says X" and "I traced X and confirmed / refuted it" are different — keep them separate in the output.
- **One simpler-alternative pass is mandatory.** Even on small changes, spend one breath asking if the whole thing is necessary. Skip only if the user explicitly says "don't question scope."
- **Don't pad with style nits when there's a structural problem.** If step 1 or step 2 surfaces a real issue, lead with it; defer nits or drop them.
- **Review before edit, always.** Asked only to simplify? Run steps 1–3 anyway, compressed, then edit. The trace is what keeps the edit safe.
- **No flattery, no hedging.** "This is a great PR but..." adds nothing. State the finding.
