---
name: tcr
description: Implement code under TCR (test && commit || revert) discipline — every change ships test+implementation together; tests pass → auto-commit, tests fail → auto-revert. Use when the user says "use TCR", "test commit revert", "implement with TCR", or wants forced-small atomic changes with no failing-code checkpoints. Pairs well with AI codegen — validate each suggestion immediately instead of letting it pile up. Trigger on /tcr.
---

# TCR — Test && Commit || Revert

`test && commit || revert`: after every change, run the tests. Pass → the change is committed. Fail → the change is **thrown away** (`git reset --hard`). There is no red phase. You cannot save failing code.

Origin: Kent Beck et al., Oslo 2018. The brutal revert is the point — it forces changes to be small, atomic, and always-green.

## Runner

The helper ships beside this file: `scripts/tcr.sh`. It does `TEST && git add -A && git commit || git reset --hard && git clean -fd`.

```bash
bash scripts/tcr.sh "<test cmd>"     # explicit test command
bash scripts/tcr.sh                  # autodetect (nvim runner/npm/cargo/pytest/just/go/dotnet/mix)
bash scripts/tcr.sh --watch "<cmd>"  # commit-on-green / revert-on-red after every save
TCR_MSG="add parser" bash scripts/tcr.sh "just test"   # custom commit message
```

Autodetect prefers this repo's nvim test runner: if `tests/run.sh` + `init.lua` exist it uses `bash tests/run.sh`. Scope to one test for fast cycles: `bash tests/run.sh <test_name>`.

## The loop the agent must follow

For **each** increment:

1. **Make ONE minimal change** — a test plus the implementation that makes it pass, together. Never a test alone; never implementation alone. Both, or neither.
2. **Run TCR**: `scripts/tcr.sh` with the project's test command.
3. **Read the verdict:**
   - `COMMITTED` → locked in. Move to the next increment.
   - `REVERTED` → the tree is back to the last green. Do **not** recover the lost code — plan a *smaller* step and try again.
4. Repeat until done. The git log becomes a heartbeat of tiny green commits.

## Non-negotiable rules

- **No failing code is ever saved.** A test commit requires the implementation that already makes it pass in the same change.
- **Smaller is the answer to a revert.** Reverted twice on a step → halve the scope again. The cost of a wrong change is bounded by how small it was.
- **Tests must be fast.** TCR is torture above a few seconds. Scope the command to the touched area for the loop; run the full suite before declaring done.
- **Don't disable/skip tests to force a green commit.** Fix the change or shrink it.
- **The revert is real.** Uncommitted work is forfeit on red — including untracked files (`git clean -fd`). Keep increments tiny.

## Difference from TDD

TDD permits a red phase (write failing test, then make it pass over minutes). TCR forbids it: feedback in seconds, atomic changes, failing tests never survive a save, commits are heartbeats not checkpoints.

## Pre-flight

- Be in a git repo with a clean (or intentionally staged) start — the first revert discards whatever is uncommitted.
- Confirm the test command exits non-zero on failure.
- `--watch` uses `fswatch` (macOS) / `inotifywait` (Linux) if present, else polls.
