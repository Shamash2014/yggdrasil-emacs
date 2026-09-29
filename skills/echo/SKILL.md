---
name: echo
description: Say it back in three parts, what, why and how, so the last thing lands. Combines wait-what, why and how. Use on /echo.
disable-model-invocation: true
---

# Echo

The owner did not get it yet. Say it back once, in three parts, short.
It combines wait-what (the re-pitch), why (the reasons) and how (the
mechanism) in one answer. It is not a new turn of work.

## What

Re-pitch the point the way wait-what does. A line of context first: what
you were doing, what you found. Then the point itself: a decision as the
decision, the reason and what it rules out; a finding as what you saw,
where, and what it means for the work.

## Why

The forces behind it: the constraint, the requirement, the decision or
the incident that shaped it. Cite where each reason comes from: a file,
a commit, an issue, a doc, the owner's words. Match the wording to the
evidence ("the commit says", "likely", "no record found"). Never infer
intent from the shape of the code alone. When the reasons need real
digging through history or tools, say so and run the why skill instead
of guessing.

## How

How it works or how it will be done: the moving parts, the path a call
or a change takes, and where it lives (path:line). Enough for a working
mental model, not an annotated listing. When it spans a subsystem and
needs exploring, say so and run the how skill.

## Rules

- ASD-STE100 Simplified Technical English: one idea per sentence, short,
  active, the same word for the same thing.
- Use the names the code and the records already use. A new name gets
  one line saying what it is.
- Keep each part to a few lines. A part with nothing to say says so in
  one line.
- Nothing new: no new work, no new proposals, no apology, no restating
  the whole turn.
