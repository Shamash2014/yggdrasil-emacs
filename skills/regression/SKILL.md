---
name: regression
description: 'Regression baselines, perf comparison and canary checks under .aob/qa/, adapted from gstack; loaded whenever a QA run compares against a prior one.'
---

# Regression

Adapted from gstack (github.com/garrytan/gstack, MIT). Our own text,
our own baseline shape.

## Surfaces

Detect which apply; a repo can have several. See references/ for
each surface's flow.

- Frontend web (references/frontend.md): package.json with a web
  framework (react-dom, next, vue, svelte, vite).
- Backend/API (references/backend.md): server entry points or an
  OpenAPI spec.
- Mobile (references/mobile.md): pubspec.yaml, a Gradle
  com.android.application plugin, Package.swift targeting iOS, or a
  .xcodeproj.

## Baselines, one set per repo under .aob/qa/

- baseline.json: QA findings and health score, shape owned by skill
  qa-health, never changed here.
- perf-baseline.json: a top-level key per surface (web, api, mobile);
  see references/baselines.md for each surface's fields.
- canary.jsonl: one append-only line per canary check, never
  rewritten.
- Screenshot sets live under .aob/qa/screens/SURFACE/.

A baseline is proposed by this skill and written only when the owner
accepts the run it came from. Never overwrite an accepted baseline
from inside a run.

## Regression mode

Rerun the full QA pass (the qa preset, with qa-health) and diff the
result against baseline.json: findings that are new, findings that
are fixed, and the score delta. New findings lead the report.

## Performance

Compare fresh numbers against perf-baseline.json per surface, gstack's
thresholds: timing REGRESSION over 50% slower or 500ms absolute,
whichever hits first; WARNING over 20%. Size REGRESSION over 25%
larger; WARNING over 10%. Request/query count WARNING over 30% more.
Mobile app size REGRESSION over 10%, WARNING over 5% (UNCONFIRMED,
ours not gstack's). See references/backend.md, references/mobile.md.

## Canary

Web and API only, gstack's rule: a single failed check is pending;
only two consecutive failed checks on the same target raise an
alert. Mobile has no live canary; a post-release store/TestFlight
smoke is the owner's call. Append every check, pass or fail, to
canary.jsonl.

## Regression test per verified fix

Once a finding is fixed and verified, write
NAME.regression-N.test.EXT next to the code's existing tests (N one
past the highest existing). Match the surrounding file's style; open
with a one-line attribution naming the finding and date. See
references/regression-test.md.

## Report

Regressions first, then warnings, then what got fixed since the
baseline, each with its evidence path.
