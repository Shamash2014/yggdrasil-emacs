# Parallel execution harness

Read this only when `~/.claude/skills/spec-mikado/mikado.py ready` returns 3+ independent nodes. Below that, do them
yourself - dispatch overhead exceeds the saving, and a single agent holding the whole graph
makes better calls than three coordinating through files.

## What is parallelizable, and what is not

| Phase | Mode | Why |
|---|---|---|
| Exploration | Single-threaded, depth-first | Each revert changes what the next attempt should be. Parallel exploration re-derives the same blocker N times. |
| Execution of ready leaves | Parallel, one subagent per node | `ready` nodes have no path between them in the DAG. Genuinely independent work. |
| Merging | Serial, one at a time | See below. This is the part that breaks if rushed. |

## Two invariants

**1. Subagents never write CHANGE.md.**

N agents appending discovered prerequisites to one file is a lost-update race, and they will
collide on node ids. Subagents return a report; the parent applies reports serially and assigns
ids itself. The graph has exactly one writer.

**2. `needs:` edges are logical, not file-level.**

Two nodes with no path between them can still touch the same code. Worktrees prevent the write
race; they do not prevent the integration break. So merge one node at a time, and after each
merge re-run **that node's `verify:` and the full suite**. If a merge breaks a node that was
already green, that is a discovered edge - add it and re-dispatch the loser.

## Dispatch

Get the set, one worktree per agent so parallel edits cannot collide:

```bash
~/.claude/skills/spec-mikado/mikado.py ready -q
```

Each node goes to one subagent with `isolation: "worktree"`. Never give one agent two nodes.

### Prompt template

> You are implementing exactly one node of a Mikado prerequisite graph.
>
> **Node:** `<id>` — <text>
> **Verdict:** `<verify command>` — this must exit 0 when you are done. It currently fails.
> **Context:** <1-3 lines: what the parent change is, what this node unblocks>
>
> Rules:
> - Implement only this node. Do not fix anything else you notice.
> - If your change fails for a reason that is not this node's own work, **revert
>   (`git stash`) and report it as a discovered prerequisite.** Do not fix it forward.
>   Discovering a prerequisite is a successful outcome, not a failure.
> - Do not edit CHANGE.md. Report; the parent owns the graph.
> - Before finishing: run `<verify command>`, then the full test suite.
>
> Return exactly this, nothing else:
> ```
> node: <id>
> outcome: green | blocked
> commit: <sha> | reverted
> discovered:
>   - name: <kebab-id>
>     needs: <comma-separated existing ids, or empty>
>     verify: <shell command that fails now and passes when this is done>
>     why: <the failure that revealed it, one line>
> notes: <one line for the ledger>
> ```
> Omit `discovered:` when outcome is green.

The "discovering a prerequisite is a successful outcome" line is load-bearing. Without it
agents read `blocked` as failure and fix forward to avoid reporting it.

## Merge protocol

Apply reports **serially**, never in a batch.

For each `outcome: green` report, one at a time:

1. Merge the worktree branch.
2. `~/.claude/skills/spec-mikado/mikado.py done <id>` — it re-runs the verdict against merged code and refuses if
   the merge broke it.
3. Run the full suite. Broke a previously-done node? That's a missing edge: record it with
   `~/.claude/skills/spec-mikado/mikado.py add`, revert this merge, re-dispatch.

For each `outcome: blocked` report:

1. For each discovered prerequisite:
   `~/.claude/skills/spec-mikado/mikado.py add <name> "<text>" --verify "<cmd>" --parent <blocked-node>`
2. Append the `notes` line to the Ledger, with the agent's `why`.
3. Confirm the branch was actually reverted. An agent that reports `reverted` but left commits
   is the fix-forward failure wearing a disguise — check before trusting it.

Then re-run `~/.claude/skills/spec-mikado/mikado.py ready` and dispatch the next wave. Two agents reporting the same
discovered prerequisite is expected and fine — `add` rejects the duplicate id, keep the first.

## Failure modes

| Symptom | Cause | Fix |
|---|---|---|
| Merged node's verify now fails | Sibling touched the same code | Missing edge. `add` it, revert, re-dispatch. |
| Agent returns a large diff for a small node | Fixed forward instead of reverting | Discard the branch. Re-dispatch with the revert rule quoted verbatim. |
| Every wave discovers the same prerequisite | It's a shared dep with edges from several parents | Correct DAG behavior — add it once, both parents point at it. |
| `ready` empty but nodes remain | Cycle, or a node marked done with open children | `~/.claude/skills/spec-mikado/mikado.py check` |
| Agents report green, suite red | Verdicts too narrow | Each node's verify must fail before its work; `arm` enforces this. |
