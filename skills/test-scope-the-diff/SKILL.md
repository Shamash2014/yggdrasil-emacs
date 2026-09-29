---
name: test-scope-the-diff
description: Map a diff to the modules, routes and flows it touches and pick a test tier. Use before any test run, or on /test-scope-the-diff.
---

# Scope the diff

Settle what needs testing before running anything: a full suite on a
one-line diff wastes time, a smoke pass on a wide one hides breakage.

## 1. Map the diff

`git diff <base>...HEAD --name-only` for the changed files. For each,
name the module, route or flow it belongs to. When the repo has a
verify skill's feature map (lat.md/features.md), match changed files
against its sections and list the features touched, not just files.
No feature map: fall back to the module/route names the codebase
itself uses (directories, route tables, package boundaries).

## 2. Pick a tier

- **quick**: a smoke pass over the affected surface only. Small,
  low-risk diff, no behaviour change to a shared or critical path.
- **full**: everything the diff touches, plus its callers (grep for
  imports/references, or the feature map's cross-references). Default
  when the diff changes behaviour, not just internals.
- **regression**: run against a saved baseline (prior green run, or a
  golden/snapshot set) when the change claims to fix or preserve
  existing behaviour. Skill regression owns the baseline diff, perf
  thresholds and canary flow for this tier.

State the tier and the one-line reason before running.

## 3. Run only that tier

Run the tests the tier selects, nothing wider. If a tier turns up
nothing runnable for a touched module, say so rather than silently
widening scope.

## 4. Say what was left out

Name any module, route or flow the diff touches that this run did not
cover, and why: out of tier, no test exists yet, or judged unaffected.
This list is the input to a coverage trace or a new test, not a thing
to bury in the run's tail.
