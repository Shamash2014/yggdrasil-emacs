# Frontend web

Drive pages with the repo's own verify skill (skills/verify-*/) when
one exists; otherwise a Playwright script against the dev server. This
is today's original regression flow; the other surfaces are new.

## What regresses

- Page timings (load/TTFB/FCP/LCP, whichever the harness reports) per
  entry in perf-baseline.json's "web" key.
- Bundle and transfer size per entry.
- Console errors: any new error on a page that had none at baseline
  is a REGRESSION regardless of timing.
- Screenshot diff, only where a visual harness already exists in the
  repo (Playwright's own snapshot compare, or another screen-testing
  tool already wired in); do not install one to get this. Compare
  against .aob/qa/screens/web/ when that directory exists.

## Thresholds

SKILL.md's Performance section, applied per page: timing REGRESSION
over 50% slower or 500ms absolute; WARNING over 20%. Size REGRESSION
over 25% larger; WARNING over 10%. Request count WARNING over 30%
more requests, per references/performance.md's worked examples.
