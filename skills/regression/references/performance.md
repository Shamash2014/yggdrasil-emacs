# Performance comparison, worked

Given perf-baseline.json entry timing_ms: 400, and a fresh measurement
of 650ms: delta is 62.5%, over the 50% threshold, so REGRESSION even
though 250ms is under the 500ms absolute leg. Either leg alone is
enough to call it.

A fresh measurement of 420ms against a 400ms baseline: 5%, under the
20% WARNING line, so PASS.

transfer_bytes 180000 baseline, 230000 fresh: 27.8%, over the 25%
REGRESSION line for size. requests 22 baseline, 27 fresh: 22.7%, under
the 30% WARNING line for request count, so PASS on that field alone.

Median of 3: when the owner asks for stable numbers, run the
measurement three times per entry and compare the median, not the
first run, against the baseline; note in the report that the median
was used.
