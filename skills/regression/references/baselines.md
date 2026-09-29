# Baseline file shapes

## .aob/qa/baseline.json

Owned by skill qa-health. Do not change its shape here; read that
skill before writing or diffing it.

## .aob/qa/perf-baseline.json

One top-level key per surface the repo has: "web", "api", "mobile".
Old repos may still have the flat shape below with no surface keys;
read that as "web" and let the next accepted run write the per-surface
shape.

    {
      "date": "2026-09-29",
      "commit": "abc1234",
      "web": {
        "entries": {
          "/checkout": {
            "timing_ms": 420,
            "transfer_bytes": 180000,
            "requests": 22
          }
        }
      },
      "api": {
        "entries": {
          "POST /orders": {
            "p50_ms": 40,
            "p95_ms": 180,
            "error_rate": 0.002,
            "query_count": 6
          }
        }
      },
      "mobile": {
        "entries": {
          "checkout-flow": {
            "cold_start_ms": 900,
            "app_size_bytes": 42000000
          }
        }
      }
    }

date and commit sit per surface when surfaces are measured on
different runs; a single top-level date/commit is fine when one run
covers all of them. Each surface's entries key is a page path (web),
a "METHOD path" pair (api), or a screen/flow name (mobile) from the
repo's own feature map. A regression run reads this file first, then
measures the same set of entries fresh, then compares each field
against the thresholds in SKILL.md and the surface's reference file.

## .aob/qa/screens/SURFACE/

Screenshot sets, one subdirectory per surface that has a visual
harness (web with one, mobile always). Each accepted baseline run
writes one file per screen or state, named for the entry key in
perf-baseline.json; a regression run captures the same names fresh
and diffs pixel sets or a screen-testing framework's own diff output
against them.

## .aob/qa/canary.jsonl

One JSON object per line, oldest first, never rewritten. Web and api
only; see references/canary.md.

    {"ts":"2026-09-29T10:00:00Z","target":"/checkout","status":"pass"}
    {"ts":"2026-09-29T10:05:00Z","target":"/checkout","status":"fail","detail":"500 on submit"}

A run reads the tail to find the last two checks per target before
deciding whether a failure is pending or confirmed.
