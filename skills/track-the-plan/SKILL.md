---
name: track-the-plan
description: "Apply when running work that outlives one turn: a project with several checkpoints, threads working in parallel, or anything a person will ask the state of. Author one plan file, then drive it through your own plan tool, which the harness reads and ticks for you."
disable-model-invocation: true
---

# Track The Plan

The state of this work lives in one file, and you write to it twice: once
when you author it, and never again by hand. After that you drive it
through your plan tool, and the harness does the editing.

**Why:** A file you keep updating by hand is a file that drifts, and
every turn spent rewriting markdown is a turn not spent on the work. The
harness already watches your plan tool. Marking an item completed there
ticks it in the file, records who ticked it and when, and does so whether
or not you remember to mention it. Prose about what you did is a claim; a
completed item in your plan tool is an event the harness saw.

## Author it once

One file named tasks.md, in this task's own directory. That is where the
harness looks first and where it writes when it has to add an item, so
using any other name splits the state in two.

A heading per checkpoint, in the order they are to be done. Under each,
one checklist item per thing that can be separately proved.

    # Cart totals match the legacy renderer

    Proof lives under .aob/evidence. Compare against reference/cart/ —
    the legacy app is the reference, not the spec.

    - [ ] line items render in the legacy order
    - [ ] tax line appears only when tax applies
    - [ ] promo discount applies before tax

Only two things in that file are read by the harness: checklist lines,
and the headings that group them. Everything else — prose, tables, links,
a paragraph on why something is stuck — is invisible to the harness and
visible to people. Write it. It costs nothing and it is how the file
stays worth opening.

## Then drive it through your plan tool

Put the same items in your plan tool and mark them completed as you
finish them. The harness reads your plan at the end of each turn, ticks
the file for you, and logs each tick against the work.

**Copy the item text.** Matching is by the words. Case, a leading number,
backticks and a full stop at the end are forgiven; nothing else is. "tax
line appears only when tax applies" ticks. "Handled the tax line" does
not, and rewording an item to sound like progress is the single most
common way to lose a tick.

**A missed match is silent.** There is no error. The item stays open, the
harness keeps holding the task for it, and the work is done. If a step
will not finish and you believe it should, suspect your wording before
anything else, and restate the item exactly as the file has it.

**Do not edit the file to tick something.** A tick the harness makes is
recorded; a tick you type is a diff. If an item is done and will not
match, say so in your reply — work you report as left over is added to
the plan as a new item automatically, which is the supported way to
correct it.

**Ticks are never undone.** Not by the harness and not by you. If
something believed done turns out not to be, that is a new item, and the
record of having believed it is worth keeping.

## What is still yours to get right

The harness can tick an item; it cannot tell whether the item was worth
having. That part does not automate.

**Items that can be proved.** If an item cannot name what would show it
is done, it is two items or it is a wish. The harness holds a step while
items are open, so an item nobody can close stops the work.

**Marking completed honestly.** Mark an item completed when the thing
that shows it is done has run and passed, not when the edit landed. The
harness will not call a step finished on a red check, but between checks
your plan tool is the only account of where the work stands.

**Saying why, where it is stuck.** A blocked item with no reason reads
like work in progress. One clause under the heading is enough, and that
prose is for people, so write it for them.

## Say how often you want to hear

A thread working well writes few progress updates, and a thread that has
gone wrong writes none either. You cannot tell them apart by waiting, so
do not try: say when you want to be told something.

Tell a thread what to report and when — after each section, on landing,
on anything that changes what another thread should do. A thread that
was never told reports when it feels like it, which is usually at the
end, which is after the moment you could have acted on it.

This is cheaper than it looks. One line in a directive costs nothing and
replaces watching, and watching does not scale past a handful of
threads anyway.

## What to carry forward

At the end of a checkpoint, write down what the next thread would
otherwise rediscover. Not what you did — the diff says that. What
surprised you: the test that needed a rebuild first, the generated file
that must not be edited, the review comment that came back twice.

Put it in the project's own instructions file, where every later thread
reads it without being told.

The harness already files half of it without being asked. When a
checkpoint passes after a check failed more than once, it writes that
down itself — which check, how many times, whether the failure was
identical each time — because it recorded all of it as it happened. Do
not repeat that. Write the part it cannot see: why the failure was what
it was, and what someone should do differently next time.
