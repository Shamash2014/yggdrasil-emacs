# UI Mode — adjustments for frontend work

Read this when the target surface is a component, page, or view. It layers onto Phases 3–6; everything else (example/golden growth, working-tree KEEP/DISCARD, mutate-from-best, plateau-break, stabilize-then-stop) is unchanged.

The default loop assumes the metric is fully deterministic. UI splits in two: **behavior and structure are property-testable and stay the metric; pixel fidelity is not a universal invariant** — it needs a second deterministic gate (visual regression) and, where "looks right" truly can't be automated, a punctuating checkpoint.

## Two-tier metric

**Tier 1 — behavioral & structural properties (the gate, deterministic).** Property-test the logic with generated props, state, and *interaction sequences* (React Testing Library / Vitest / Playwright component tests + fast-check over jsdom or a real browser). These are genuine property tests, so they remain the acceptance metric:

- **State-machine invariants** — for any generated sequence of interactions, the component never reaches an invalid state (never two modals open; submit disabled while the form is invalid; focus is never lost; a spinner always resolves to data or error).
- **Data→DOM mapping** — rendering N items yields N rows; empty data → the empty state, never a crash; an error prop → the error UI.
- **Accessibility invariants** — every interactive element has an accessible name + role; no duplicate ids; focus order is monotonic; `axe`/`jest-axe` reports zero violations (fully deterministic — strong property).
- **Responsive invariants** — across a generated range of viewports, no horizontal overflow; touch targets ≥ 44px; text is never clipped.
- **Controlled-input roundtrip** — `onChange(v)` then re-render with `v` shows `v`. **Render idempotence** — same props ⇒ identical DOM.

**Tier 2 — visual fidelity (a second deterministic gate).** Pixel/layout correctness is not a universal invariant, so use **visual-regression with golden snapshots** (Playwright screenshots / jest-image-snapshot / Storybook stories). The example loop maps directly: a diff over threshold is the counterexample; the **approved screenshot is the golden** that gets frozen. Goldens only grow — *properties discover, goldens pin*, exactly like example tests. The visual gate is green only when every golden matches within threshold. Treat a golden as another entry in the per-test result vector (`best_results.json`): it has a name and a pass/fail, scored like any example.

## The fidelity oracle — where determinism runs out

"Matches the design" cannot be fully automated. In order of preference:

1. **Token oracle (deterministic — keep in Tier 1)** — if a Figma source or design tokens exist, assert measurable specs: spacing, color, font size/weight/line-height, and radius equal the token values. This is checkable, so it stays a property. Pair with the `figma-build-design` skill to fetch the design context.
2. **Golden approval (the one sanctioned human checkpoint)** — the first time a screen is built or intentionally restyled, the loop **pauses** for the user to approve the rendered screenshot as the golden. After approval it resumes fully autonomous and gates against that golden with no human in the loop. Visual taste is the user's call, not the agent's — so this is the single allowed interruption to autonomy.
3. **LLM-judge (advisory only, never a gate)** — screenshot vs design description, judged by you. Non-deterministic, so it only flags candidates for the human checkpoint; it never decides KEEP/DISCARD.

## Flakiness control (mandatory for the visual gate)

A flaky visual gate is worse than none. Before every screenshot: disable animations/transitions, freeze time and `Math.random`, wait for fonts + network-idle + layout-stable (never a fixed sleep), pin viewport and device-pixel-ratio, and mask known-dynamic regions (timestamps, avatars, randomized ids). The flaky-is-red rule applies doubly: a golden that diffs intermittently is treated as red until stabilized.
