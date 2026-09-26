---
name: ice
description: "Run a change the ICE way (Intent, Context, Expectation): turn a free-form ask or an open OpenSpec change into a confirmed intent, owner-locked checks and tracer-bullet tasks, then verify every item with ice-verify. Apply in a repo wired for ICE (it has .ice/ and openspec/ with the ice schema), or when the owner asks for ICE; a lead running workers applies it before any code."
---

# ICE

Intent and expectations belong to the owner; context is looked up as
needed; the building agent never defines done. This skill is the order
the work goes in and the check that closes each item. Whoever runs it
delegates the reading and writing to workers and does none of it.

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
- perf_cmd and mutate_cmd are set only when the repo has tools for them:
  a benchmark it already runs, a mutation tool for its language.

## What the lead does, and what stays the owner's

You run the context; Emacs commands are for rare manual use. Through
workers, never by hand:

- gaps: worker-gaps writes CHANGE/gaps.md (step 3);
- learnings (ice-learnings): record the owner's feedback and any finding
  that recurs twice, and paste the lines that apply into every brief's
  CONTEXT;
- rules: when a learning keeps holding, propose a rule for
  lat.md/rules.md; the owner approves it before a worker writes it;
- the map: when a change's last item is ticked, a worker updates the
  lat.md feature sections the change touched, a maintain pass scoped to
  that change;
- archive: after the owner archives, run .ice/ice-archive-to-lat.

Two acts are the owner's alone and never yours: confirming the intent
(SPC a k R) and approving the checkpoints (SPC a k A).

## Before any code

1. Open the change: openspec new change SLUG. The ice schema is the
   default in a wired repo.
2. Explore: read the code; probe and prototype, in a scratch folder,
   whatever running something can settle.
3. Intent: draft intent.md from the ask and what exploration found. Then
   run the gap detector, a separate read-only pass under the gaps preset
   (worker-gaps when a lead runs workers) that writes only
   CHANGE/gaps.md. Settle what a probe can; ask the owner only what is
   left, each with its default.
4. Restate: write the Restated section in your own words. The owner
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
   slice, "N. few words" (at most 8), never checkboxes. When
   ice-prototype applies, checkpoint 1 is the prototype. The owner
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
with every scenario covered, then the live, perf and mutation lanes the
repo sets, and ends on one line:

- unit-verified or live-verified, with the diff on disk and inside the
  item's scope: the item may be ticked;
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

When the last item is ticked, a worker updates the lat.md feature
sections the change touched. The owner archives the change (openspec
archive); then run .ice/ice-archive-to-lat on the archived folder. It
files the change under its feature in lat.md, deletes
docs/prototypes/CHANGE/, and moves the change's learnings lines into
lat.md/learnings.md, "owner N" rewritten to "owner CHANGE#N", all but
those whose Where is "this change".
