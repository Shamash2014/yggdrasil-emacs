---
name: spec-mikado
description: Use when a change's prerequisites are unknown until you attempt it - legacy refactors, cross-cutting migrations, "I changed X and six unrelated things broke", dependency untangling, or work that keeps growing once started. Also use when the user mentions the Mikado method, asks to map prerequisites before refactoring, or wants a spec that survives discovery instead of freezing before implementation. Not for greenfield features whose steps are already known - those go to gsd or writing-plans.
---

# Spec-Mikado

## Overview

Ordinary spec-driven development writes the contract, freezes it, then implements. That works
only when the steps are already known. On a change whose prerequisites surface *while* you
attempt it, the frozen spec is wrong by the second hour and quietly abandoned.

Spec-Mikado fuses the two artifacts that are normally separate: **the prerequisite graph IS the
spec.** Every node is a requirement, every requirement carries the command that proves it, and
discovery rewrites the contract instead of invalidating it.

It is a DAG, not a tree — one prerequisite is commonly shared by several parents, and that
sharing is what makes the ordering non-obvious. `mikado.py` in this skill directory parses it,
enforces the gates, and computes which nodes are workable right now.

Two laws:

1. **A failed attempt is reverted, never repaired.** The failure is the finding; the broken
   code is not.
2. **A node without a runnable verdict is not a node.** Enforced by the harness, not by taste.

## When to use

- Prerequisites are unknown until you try: "just rename this" that cascades
- Cross-cutting migration (swap a library, split a module, change a core type)
- Legacy code where the blast radius is unmeasured
- Any change already attempted once and abandoned half-done

## When NOT to use

- Greenfield feature with known steps -> `superpowers:writing-plans` or `gsd:plan-phase`
- A bug with an identified root cause -> `superpowers:systematic-debugging`
- Mechanical find-and-replace with a green suite -> `refactor` or `tcr`
- Single-file edits

## Law 1: revert, never repair

```
git stash        # or: git checkout -- .
```

Run it the moment the naive attempt fails. Not after "one quick fix."

**No exceptions:**
- Not when the fix is one line
- Not when you are "already most of the way there"
- Not when you want to keep it "just to look at" - stash it and read the stash
- Not when the graph is small enough to hold in your head
- Not when the same prerequisite appears twice

Fixing forward is how a two-node graph becomes an eleven-file uncommittable diff.

## Law 2: every node carries its verdict

A node's `verify:` is a shell command. Exit 0 means done. That is the whole definition.

Writing `verify:` is the design work — it forces the node to name an observable outcome before
you know how to reach it. A node you cannot write a command for is not understood well enough
to attempt; explore it further or split it.

The harness enforces this with **arming**: before any work, a node's verdict must be observed
*failing*.

```bash
~/.claude/skills/spec-mikado/mikado.py arm export-fields     # refuses if verify already exits 0
~/.claude/skills/spec-mikado/mikado.py done export-fields    # refuses unless armed and now passing
```

A verdict that cannot fail proves nothing. `arm` is what stops `npm run build` from being
accepted as the verdict for a CSS change.

## Workflow

### 0. Contract gate — no implementation code until this passes

Run every command below from the repo root, where `CHANGE.md` lives. The harness path is
absolute on purpose — do not set a shell variable for it, since shell state does not survive
between tool calls. If the path below is missing, the skill was not installed via `npx skills`;
use `~/.emacs.d/skills/spec-mikado/mikado.py` instead.

Write `CHANGE.md` (format below): goal, invariants, done-when, and the goal node. Then:

```bash
~/.claude/skills/spec-mikado/mikado.py check      # structure: cycles, missing deps, orphans, missing verdicts
~/.claude/skills/spec-mikado/mikado.py arm goal   # the goal's verdict must fail right now
```

Both must pass. Do not proceed while the goal names a solution instead of an outcome, or while
any invariant lacks a `verify:`.

**The goal's `verify:` is its done-when condition, not the test suite.** You start from green,
so `go test ./...` alone exits 0 and `arm goal` will refuse it — correctly. Give the goal a
verdict that is false today: `! test -f transport/legacy_encoder.go && go test ./...`

### 1. Exploration — discover the graph

Single-threaded and depth-first. Each revert changes what the next attempt should be, so
parallel exploration just re-derives the same blocker N times.

Establish green first — actually run the suite, don't assume.

```
1. Attempt the current node naively, the simplest way that could work.
2. Green?  -> commit, then: ~/.claude/skills/spec-mikado/mikado.py done <id>
3. Red?    -> read the failures. Each distinct cause is one prerequisite.
4. REVERT (git stash). Non-negotiable.
5. ~/.claude/skills/spec-mikado/mikado.py add <name> "<text>" --verify "<cmd>" --parent <current>
6. Append one line to the Ledger.
7. Recurse into a child. It is now the current node.
```

Depth-first: one deep path finds the real blocker faster than three shallow ones. Explore a
sibling only when the current path bottoms out. Exploration ends when `~/.claude/skills/spec-mikado/mikado.py ready` returns
nodes that go green on their own.

### 2. Execution — ready nodes only, deepest first

```bash
~/.claude/skills/spec-mikado/mikado.py ready
```

Everything it lists has all its prerequisites satisfied. Per node: `arm`, implement, `done`
(which re-runs the verdict), full suite, one commit.

A "ready" node that fails on attempt was never a leaf — revert, add its children, continue. This
is normal and is not a planning failure.

**Parallel dispatch:** when `ready` returns 3+ nodes, one subagent per node in its own worktree
— read `HARNESS.md` for the dispatch prompt and the serial merge protocol. Below 3, do them
yourself. Never dispatch during exploration.

### 3. Close

When the goal's verdict passes, move `CHANGE.md` to `docs/changes/<name>.md`. It now records
what the change actually required, including the paths that failed — the ledger is the part no
other format keeps.

## CHANGE.md format

Flat nodes with explicit `needs:` edges — a nested checklist cannot express a shared
prerequisite. Ids are kebab-case. `state:` is managed by the harness; don't hand-edit it.

```markdown
# Change: replace hand-rolled JSON encoder with encoding/json

## Contract
Goal: transport layer serializes via a swappable codec, default encoding/json
Invariants:
  - wire format byte-identical for existing payloads
    verify: go test ./transport/ -run TestWireCompat
Done when:
  - legacy_encoder.go is gone and the suite is green
    verify: ! test -f transport/legacy_encoder.go && go test ./...

## Nodes

### goal: swap the encoder in transport/
needs: export-fields, drop-unsafe
verify: ! test -f transport/legacy_encoder.go && go test ./...
state: open

### export-fields: export Payload fields
needs: rename-body
verify: go build ./...
state: armed

### rename-body: rename payload.body -> payload.Body (23 call sites)
needs:
verify: go build ./...
state: done

### drop-unsafe: drop the unsafe offset math in fastpath.go
needs:
verify: go test ./transport/ -run TestFastPath -race
state: open

## Ledger
- attempt 1: swapped encoder directly -> 14 failures, all unexported field access. reverted.
- attempt 2: exported fields first -> fastpath.go computes offsets via unsafe. reverted.
- attempt 3: tried to special-case the unsafe path -> abandoned, became drop-unsafe.
```

**The ledger is prose and it is not optional.** It stops you re-walking a known-dead path three
context windows later, and it is the only record of *why* the graph has the shape it does.

## Quick reference

| Step | Command |
|---|---|
| Validate structure | `~/.claude/skills/spec-mikado/mikado.py check` |
| Progress + what's blocked | `~/.claude/skills/spec-mikado/mikado.py status` |
| Workable now | `~/.claude/skills/spec-mikado/mikado.py ready` (`-q` for ids) |
| Before starting a node | `~/.claude/skills/spec-mikado/mikado.py arm <id>` |
| After finishing a node | `~/.claude/skills/spec-mikado/mikado.py done <id>` |
| Record a discovery | `~/.claude/skills/spec-mikado/mikado.py add <id> "<text>" --verify "<cmd>" --parent <id>` |
| Visualize | `~/.claude/skills/spec-mikado/mikado.py graph` (mermaid) |
| Attempt failed | `git stash`, then `add`, then ledger line |
| 3+ ready nodes | See `HARNESS.md` |

Exit codes: 0 ok, 1 gate refused or check failed, 2 malformed graph.

## Rationalizations

| Excuse | Reality |
|---|---|
| "The fix is two lines, reverting is wasteful" | Two lines is how every unrevertable diff starts. The graph is the product, not the diff. |
| "I'll write the contract after I know what's possible" | Exploration without a goal cannot terminate. The contract defines done. |
| "Let me note the prerequisite and keep going" | Then you are debugging two changes at once and can attribute neither failure. |
| "It's all really one prerequisite" | Distinct failure causes are distinct nodes. Merging them hides the ordering. |
| "Top-down is faster, the leaves are obvious" | Obvious leaves are cheap to confirm. Wrong leaves cost the whole path. |
| "I'll add the verify command later" | Later you will accept the outcome you got. The command is the design work. |
| "'All tests pass' is a fine verify" | Never, not even for the goal. A green suite passes before you start, so `arm` refuses it. The goal's verdict is its done-when condition. |
| "Skip arming, I know it fails" | Then arming costs one command and confirms it. Most "obviously failing" verdicts pass. |
| "Fan out, it's faster" | Only for ready nodes, 3+, in worktrees. Parallel exploration re-derives one blocker N times. |
| "Tests were green a minute ago" | Verify green before each attempt, or a stale failure gets recorded as a prerequisite. |

## Red flags - stop and revert

- Editing a second file to make the first attempt work
- A `TODO` added mid-attempt
- "While I'm in here..."
- `CHANGE.md` unchanged after a failed attempt
- Uncommitted work spanning more than one node
- A node moved to `done` without `arm` having refused first
- A subagent that reports `reverted` but left commits on its branch
- Implementation code written before the contract gate passed

**All of these mean: `git stash`, record the node, restart from green.**

## Common mistakes

**Recording symptoms instead of prerequisites.** "TestFoo fails" is not a node. "`Foo` depends
on the concrete writer type" is. The symptom goes in the ledger; the cause becomes the node.

**Skipping the naive attempt** because the prerequisite seems obvious. The attempt is the
measurement. A guessed graph is a plan, and plans are what this method replaces.

**Treating `needs:` as file-level independence.** It encodes logical prerequisites. Two ready
nodes can still touch the same code — that's why merges are serial and re-verified.

**Letting the graph go stale.** Run `add` in the same breath as the revert, before anything
else. A graph written from memory at the end of a session is fiction.
