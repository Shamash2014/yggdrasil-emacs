---
name: prototype
description: Build a throwaway prototype when running something settles a question faster than discussing it - UI, spike, mock, dry run, or CLI walk-through.
---

# Prototype

A question a prototype can answer is answered that way, not asked.
Before you build, name the one question this settles. If you cannot
name it in a sentence, you are not ready to build.

## Kinds

- UI: static screens and states. Inside ICE, hand this off to
  ice-prototype; it is the UI specialist and knows the owner's
  approval and the reference the UI gate needs.
- Spike: the smallest code that proves feasibility, that an API or
  library behaves as hoped, or a performance bound.
- API or contract mock: a fake server or stub that lets a caller be
  written against a shape before the real one exists.
- Data or migration dry run: run the transform against real or sample
  data and look at what comes out, before it touches anything live.
- CLI or flow walk-through: drive the sequence a person or script will
  run and see where it breaks or reads wrong.

## Rules

- Name the question before starting. One sentence.
- Timebox it. A prototype that grows a backlog has become a build.
- Build it throwaway: docs/prototypes/CHANGE/ inside an ICE change, or
  a scratch dir or a throwaway git worktree the owner approves
  otherwise. Never on the main working tree's production paths.
- Never merge or promote the prototype's code into production. The
  real build starts clean from what it proved; copying the throwaway
  code in is how its shortcuts survive.
- When the owner is choosing between directions, build several: label
  each, keep them switchable, and let the owner pick from evidence,
  not description.
- End on evidence: a screenshot, output, or measurement, whatever the
  question needs to be answered by looking. Then one verdict line:
  proved, disproved, or changed the plan.
- The owner decides what happens next: build for real, try another
  direction, or drop it.
- Clean up: delete the scratch dir or throwaway worktree once the
  owner has the verdict and, for an ICE change, once the checkpoint
  that used it is done; ice-archive-to-lat already deletes anything
  left under docs/prototypes/CHANGE/ on archive.
