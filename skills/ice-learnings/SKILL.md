---
name: ice-learnings
description: "Keep what an ICE change learns: owner feedback and review findings that recur become dated lines in CHANGE/learnings.md and lat.md/learnings.md, the lines that apply go into every later brief's CONTEXT, and a line that keeps holding graduates into a rule in lat.md/rules.md or a check. Apply whenever the owner gives feedback on a checkpoint, a review finding repeats, or a lead writes a brief."
---

# ICE learnings

Every checkpoint the owner corrects teaches something the next one
should not have to be told. Written down, the lesson reaches every later
brief; kept, it hardens into a rule or a check. Early checkpoints need
the owner most; later ones need them less because of this file.

**Why:** Helix records every engineer's feedback and every repeated
finding, and its loop grows more autonomous as the memory fills. Task
context in the brief beats procedure (arXiv 2603.17973); rules should
record why they exist (2608.11095), and a rule turned into a check is
followed 88.3% of the time against 67.0% (2603.00822).

## The files

- CHANGE/learnings.md: this change only, from the template
  etc/ice/lat/change-learnings.md;
- lat.md/learnings.md: the whole project, from etc/ice/lat/learnings.md.

One line per lesson, under "## Lines", newest last:

    - 2026-09-26 | owner 3 | totals show the currency symbol after the amount in de-DE | src/cart/**

Date, source, lesson, where it applies. The source is "owner N" for
feedback on checkpoint N, or "review R4 x2" for a finding that recurred.
In the project file the owner source names the change: "owner cart-promo#3".
Where is paths or globs, a feature name from lat.md/features.md, all,
or "this change" for a lesson that holds for this change only.
The lesson is one sentence a builder can act on, in the owner's terms.

## When a line is written

- The owner gives feedback on a checkpoint that is not only about that
  checkpoint: a taste, a convention, a trap. One line per point.
- A review finding recurs: the same rule id and the same kind of breach
  in two items of one change, or in two changes. Once is a fix; twice is
  a lesson.
- A UI review blocker recurs the same way.

Not a line: a one-off bug, anything already in rules.md, and anything
the owner has not said or a review has not found twice. A builder's own
opinion never becomes a line.

A build worker writes the line, briefed with the exact text; whoever
runs ICE decides it and does not edit files itself.

## Into every brief

Before sending any brief for this change, the lead reads both files and
pastes into the brief's CONTEXT every line whose Where covers a path the
brief names, or the feature it touches, or all. Paste the lines as they
stand, under "Learnings:". Never summarise them, and never paste lines
that do not apply: a long list is read as noise.

Reviewers get the same lines beside the rules; a line is not a rule and
a breach of one alone is never a blocker.

## Graduating

At archive, .ice/ice-archive-to-lat moves every line of
CHANGE/learnings.md to lat.md/learnings.md, "owner N" rewritten to
"owner CHANGE#N", except the lines whose Where is "this change"; those
are thrown away with the change.

A project line graduates when it has held for two changes, or the owner
says so:

- into a check, when a script or test can tell a breach: grep, lint rule,
  test. Prefer this;
- otherwise into lat.md/rules.md as a new "do not" rule, the next free
  id, with the line's lesson as its why and its Where as the scope.

Rules are the owner's: propose the rule or check, and write it only once
the owner agrees. Then end the line with " | graduated: R7" or
" | graduated: check PATH"; the line stays, so the history is kept, and
is no longer pasted into briefs.

## Done

A lesson is kept when its line is in the right file in the format above.
This prints nothing:

    grep '^- ' CHANGE/learnings.md lat.md/learnings.md \
      | grep -v -E '^[^:]+:- [0-9]{4}-[0-9]{2}-[0-9]{2} \| (owner|review) [^|]+ \| [^|]+ \| [^|]+( \| graduated: [^|]+)?$'
