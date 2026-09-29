---
name: qa-health
description: 'Health score, regression baseline, real-user filter and claims table for a QA run; loaded from the qa preset before writing qa-proposals.md.'
---

# QA health, baseline and claims

## Health score

0 to 100. Start the severity score at 100 and subtract per finding:
blocker 20, major 8, minor 2, floored at 0; harness issues never count.
Blend with the pass rate of the scenarios exercised this run:

    score = round(0.7 * severity-score + 0.3 * 100 * pass-rate)

## Baseline

.aob/qa/baseline.json holds the last accepted score and findings list,
one per repo. Create it, empty, {"score": null, "findings": []}, only
when absent; never overwrite an existing one. Read it first and report,
ahead of everything else, the delta against it: every finding that is
new or has grown more severe since the baseline, and the score change.
The file is updated only when the owner accepts this run, never by the
QA worker itself; the report carries the proposed new JSON for the
owner to write.

## Real-user filter

Before a finding is reported as a bug: would a person hit this exact
failure through the product, never through the test's own selector,
timing or fixture? A failure that lives in the harness (a stale
selector, a race the app does not have, a fixture unlike production
data) is a harness issue, listed on its own, never as a finding and
never counted against the health score.

## Regression runs

A run that compares against baseline.json, checks performance, or
watches a canary loads skill regression for the flow and thresholds.

## Claims table

The report ends on a table, one row per claim from the intent or the
brief's ACCEPTANCE lines: the claim, what was observed, pass or fail,
the evidence file. A claim nothing checked is fail, not omitted.
