---
name: track-the-plan
description: "Apply when running work that outlives one turn: a project with several checkpoints, threads working in parallel, or anything a person will ask the state of. Author one plan file, then drive it through your own plan tool, which the editor mirrors into the session's tasks.md."
disable-model-invocation: true
---

# Track The Plan

The state of this work lives in one file, and you write to it twice: once
when you author it, and never again by hand. After that you drive it
through your plan tool, and the editor carries it into the file.

**Why:** A file you keep updating by hand is a file that drifts, and
every turn spent rewriting markdown is a turn not spent on the work. The
editor mirrors your plan tool into the session's tasks.md: an entry you
mark completed ticks its item, and an entry the file lacks is added, whether
or not you remember to mention it. Prose about what you did is a claim; a
completed item in your plan tool is an event the editor saw.

## Author it once

One file named tasks.md, made with todo_write, which binds it to this
session. That is the file the editor mirrors your plan into, so a list
kept anywhere else splits the state in two.

A heading per checkpoint, in the order they are to be done. Under each,
one checklist item per thing that can be separately proved.

    # Cart totals match the legacy renderer

    Proof lives under .aob/evidence. Compare against reference/cart/ —
    the legacy app is the reference, not the spec.

    - [ ] line items render in the legacy order
    - [ ] tax line appears only when tax applies
    - [ ] promo discount applies before tax

The list holds headings and checklist items and nothing else, and it
changes only through the todo tools. Context — why an item exists, what
blocks it, the reference to compare against — goes in the item's own
words or in the task's other documents (the proposal, the design), not
as prose in the list.

## Then drive it through your plan tool

Put the same items in your plan tool and mark them completed as you
finish them. The editor mirrors every plan update into the session's
tasks.md: completed entries tick, new entries are added. You can also use
todo_list, todo_add, todo_update and todo_remove directly.

**Copy the item text.** Matching is by the words. Case, a leading number,
backticks and a full stop at the end are forgiven; nothing else is. "tax
line appears only when tax applies" ticks. "Handled the tax line" does
not, and rewording an item to sound like progress is the single most
common way to lose a tick.

**A missed match is silent.** There is no error. The item stays open, the
harness keeps holding the task for it, and the work is done. If a step
will not finish and you believe it should, suspect your wording before
anything else, and restate the item exactly as the file has it.

**Do not hand-edit the file.** Tick through your plan tool or
todo_update; a tick you type is a diff the editor was never told about.
If an item is done and will not match, tick it with todo_update, using
the id todo_list gives it.

**Ticks are never undone.** Not by the harness and not by you. If
something believed done turns out not to be, that is a new item, and the
record of having believed it is worth keeping.

## What is still yours to get right

The harness can tick an item; it cannot tell whether the item was worth
having. That part does not automate.

**Items that can be proved.** If an item cannot name what would show it
is done, it is two items or it is a wish. An item nobody can close
stays open, and the list keeps saying the work is unfinished.

**Marking completed honestly.** Mark an item completed when the thing
that shows it is done has run and passed, not when the edit landed.
Nothing checks that for you: between checks, the list is the only
account of where the work stands.

**Saying why, where it is stuck.** A blocked item with no reason reads
like work in progress. Reword it with todo_update to say what blocks it,
in one clause.

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

Write what a later thread could not see from the code: why the failure
was what it was, and what someone should do differently next time.
