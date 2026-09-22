---
name: refactor
description: Execute an autonomous refactor with pilot-then-batch protocol
---

# Refactor

Execute an autonomous refactor with the following contract:

## Goal

[describe transformation, e.g., 'replace all encoding/json imports with goccy/go-json across services/']

## Success Criteria

- All existing tests pass
- Benchmark suite shows no regression >5% on any metric
- No new lint warnings

## Protocol

1. Inventory all candidate files via Grep. Report count.
2. Pick a representative pilot file. Apply the transformation. Run tests + benchmarks. If any criterion fails, diagnose and fix or abort with explanation.
3. Once pilot succeeds, batch-apply across remaining files in chunks of 20 using ctx_batch_execute.
4. After each chunk: run tests. If failures, bisect to identify the offending file, revert just that file, log it, continue.
5. Final: run full test + benchmark suite. Produce a report with files changed, files skipped (with reasons), perf deltas, and a single commit per logical chunk.

Do not ask for confirmation between chunks unless success criteria are violated. Use TodoWrite to track progress.
