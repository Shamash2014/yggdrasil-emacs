---
name: ice
description: Run a change the ICE way (Intent, Context, Expectation). Apply in a repo wired for ICE (.ice/, openspec/), when the owner asks for ICE, or as a lead.
---

# ICE

Intent and expectations belong to the owner; context is looked up as
needed; the building agent never defines done. This skill is the order
the work goes in and the check that closes each item. Whoever runs it
delegates the reading and writing to workers and does none of it.

Code is the final spec. ICE changes code only through an item's diff and
never regenerates or rewrites it from intent, expectations, spec deltas,
a prototype, openspec/specs or lat.md. Those stay minimal: intent, why,
invariants and anchors, never the code restated. lat.md links to the
code one way, [[path#symbol]] where lat parses the language and `path`
elsewhere; the code never carries @lat comments.
Where a spec or lat.md disagrees with the code, the spec or lat.md is
corrected, or the owner opens a change.

## Is the repo wired

Wired means .ice/ and openspec/ exist and openspec/config.yaml names the
ice schema. If not, ask the owner to wire it: importing the project (SPC
p i in Emacs) with the ice extra picked, or M-x ygg-ice-wire. If the owner says no, this skill does not apply; work from a
plan file instead.

## Wired

Wiring fills .ice/config from the repo's own test runner (test_cmd with
{file} and {filter}, report_path, lock_paths) and runs the whole suite
once as the baseline, .ice/state/baseline.json: passed, failed or
broken. A red or broken baseline is the owner's to settle before the
first change. A linked worktree reuses the project's .ice/config and
records its own baseline the first time it is wired.

Then, once per repo, you see to verification through workers:

- If the repo has no verification skill (skills/verify-*), a build
  worker creates one with create-verification-skill, its feature map in
  lat.md/features.md. Run it once and have the worker record the result
  in .ice/state/verify-baseline.json beside the baseline.
- Its entry command becomes live_cmd in .ice/config, and it is where the
  UI gate's screenshots come from.
- sec_cmd is the verify skill's security step, set only when it has one.
- perf_cmd is set only when the repo has a benchmark it already runs.
  mutate_cmd is filled automatically when wiring finds the language's
  mutation tool installed (mutmut for pytest, Stryker for Vitest/Jest,
  cargo-mutants for Cargo, gremlins or go-mutesting for Go), scoped to
  the files changed since the locked base; when no tool is installed,
  ice-wire prints the install command instead and leaves mutate_cmd
  unset, the owner's call.
- commit_gate = off in .ice/config, set by the owner only, turns off the
  pre-commit hook ice-wire installs: it reads .ice/ledger.tsv and
  refuses a commit touching a locked change's files (its folder, its
  lock record, and every slice's Files: line in tasks.md) unless a
  unit-verified or live-verified row exists for that exact content; it
  never runs tests itself, only hashes the staged tree and reads the
  ledger, and git --no-verify skips it the way it skips any hook.

## What the lead does, and what stays the owner's

You run the context; Emacs commands are for rare manual use. Through
workers, never by hand:

- gaps: worker-gaps writes CHANGE/gaps.md (step 3);
- learnings (ice-learnings): record the owner's feedback and any finding
  that recurs twice, and paste the lines that apply into every brief's
  CONTEXT;
- rules: when a learning keeps holding, propose a rule for
  lat.md/rules.md; the owner approves it before a worker writes it;
- the map: when a change's last slice is removed, a worker updates the
  lat.md feature sections the change touched, a maintain pass scoped to
  that change;
- archive: after the owner archives, run .ice/ice-archive-to-lat.

Two acts are the owner's alone and never yours: confirming the intent
(SPC a k R) and approving the checkpoints (SPC a k A).

## Before any code

1. Open the change: openspec new change SLUG. The ice schema is the
   default in a wired repo.
2. Explore: read the code; probe whatever running something can
   settle, or prototype through skill prototype (ice-prototype for
   UI) under docs/prototypes/CHANGE/, rather than asking the owner.
3. Intent: draft intent.md from the ask and what exploration found. Then
   run the gap detector, a separate read-only pass under the gaps preset
   (worker-gaps when a lead runs workers) that writes only
   CHANGE/gaps.md. Settle what a probe can; ask the owner only what is
   left, each with its default.
4. Restate: write the Restated section with the restate skill (Goals,
   Problem, Not the goal, Unsure), in your own words. The owner
   confirms it (SPC a k R writes the Confirmed date and a sha1 of What is
   wanted and Restated). Never write the Confirmed line, and never take a
   chat reply as the confirmation. Wait until ice-check intent passes; an
   edit to either section after that fails it until the owner confirms
   again.
5. Checks: a fresh worker that never sees an implementation drafts
   expectations.md and the checks under the ice-checks skill. The owner
   confirms them. .ice/ice-fail-on-base CHANGE --red must pass: every
   scenario has a check and every check fails now, before any
   implementation. Then the owner runs .ice/ice-lock CHANGE lock, which
   records the commit the checks were locked against; nobody else locks,
   and a re-lock (--force) refuses without ICE_LOCK_OWNER=1 set by the
   owner. The full fail-on-base, each check failing on that locked base
   and passing on the working tree, runs inside ice-verify.
6. Tasks: tasks.md opens with "## Checkpoints", one numbered line per
   slice, "N. few words" (at most 8), never checkboxes. When a
   prototype applies, checkpoint 1 is the prototype: ice-prototype for
   user-visible UI, skill prototype otherwise. The owner
   approves the list with SPC a k A, which writes "Approved: YYYY-MM-DD
   sha1:XXXXXXXX" under it. Never write that line, and never take a chat
   reply as approval; an edit to the list after it fails the plan check
   until the owner approves again. The detailed slices follow under
   "## Slices": tracer-bullet slices, each with Blocked by, Files,
   Scenario, Pass when and Evidence. ice-check plan passes.

## Each item

The VERIFY for an item of an ICE change is ice-verify CHANGE, run by
whoever judges the item, never by the worker that built it; prefer the
copy in the Emacs checkout (etc/ice/ice-verify, run from the repo), since
a worker can edit .ice/ice-verify. It runs the intent check, the plan
lint, the lock on the owner's checks, the .ice scripts, the config and
the test support, fail-on-base against the locked base, the unit tests
with every scenario covered, then the sec, live, perf and mutation lanes
the repo sets, and ends on one line:

- unit-verified or live-verified, with the diff on disk and inside the
  item's scope: the item may close;
- failed STEP or blocked STEP: the item stops there; say which step, the
  choices and a pick, and let the owner decide.

After ice-verify passes an item, in this order:

1. The UI gate (ice-ui-review, the ui worker) when the item changes
   anything a user sees.
2. The review loop (ice-review-loop, the review worker): two reviewers,
   fixes, until both approve or the cap. etc/ice/ice-check reviews
   CHANGE N passes when the last round of CHANGE/reviews/code-N.md has
   two approvals and, when ui-N.md exists, its last verdict is approve.
3. The owner. Their feedback becomes lines under ice-learnings.

When the item closes, its code is written, so its slice leaves tasks.md:
run etc/ice/ice-verify --done N CHANGE. It removes slice N only when a
verified row matches the current content, and the ledger keeps the
slice with its evidence and Files, which ice-check, the commit gate and
ice-archive-to-lat read as done. Never tick a slice and keep it. The
Checkpoints list stays as the owner approved it. The removal moves the
content hash, so ice-verify runs once more before the owner commits.

Every brief's CONTEXT carries the learnings lines that apply to it,
pasted as they stand.

A failed lock means the checks changed under the worker. Never re-lock
to get past it; that is the owner's. Workers do not commit, so the
commit stays the same from item to item: only the verdict line of the
run just made on this item's diff counts, never an older ledger row.
Each run writes a row to .ice/ledger.tsv and keeps its logs under
.ice/evidence/CHANGE/SHA/RUN/; name the worker and the files it changed
beside each row when reporting.

## After

When the last slice is removed, a worker updates the lat.md feature
sections the change touched from the merged code, each claim anchored
to the code, until lat check and .ice/ice-lat-drift CHANGE are clean; a
section left alone on purpose is listed in design.md (intent.md is
locked) as "lat unchanged: [[section]] (why)". The owner archives the
change (openspec archive), which merges its deltas into openspec/specs;
then run .ice/ice-archive-to-lat on the archived folder. It
files the change under its feature in lat.md, deletes
docs/prototypes/CHANGE/, and moves the change's learnings lines into
lat.md/learnings.md, "owner N" rewritten to "owner CHANGE#N", all but
those whose Where is "this change".

Then a worker prunes what the change touched in openspec/specs and
lat.md to intent, why, invariants and anchors: what restates the code
goes, and what disagrees with the code is corrected, never the code.
