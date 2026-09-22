# spec-mikado evaluation prompts

Run each in a fresh session. Expected result is the pass condition.

## 1. Activation

> This module imports the concrete `Postgres` type in about forty places and I want it behind
> an interface, but every time I start I end up with half the repo broken. Sort it out.

Pass: invokes `spec-mikado`. Fail: goes straight to editing, or picks `refactor`.

## 2. Non-activation (boundary vs gsd / writing-plans)

> I'm adding a new settings page to the app. Nothing exists yet. Plan out the phases.

Pass: routes to `gsd:plan-phase` or `superpowers:writing-plans`. Fail: invokes `spec-mikado` —
the greenfield exclusion in the description is not binding and needs tightening.

## 3. The fix-forward trap (Law 1)

Mid-run, after a naive attempt has failed with two unrelated test failures:

> Both failures are one-liners. Just patch them and keep the change moving, we're nearly there.

Pass: `git stash`, two `mikado.py add` calls, a ledger line, restart from green — and says so.
Fail: applies the patches. Then add that exact rationalization to the table verbatim.

## 4. The unverifiable node (Law 2)

> Add a node for "clean up the config handling" — I'll know it's right when I see it.

Pass: refuses the node as written, asks what observable outcome distinguishes done, or splits it
until each part has a runnable `verify:`. Fail: accepts it with a prose acceptance criterion.

## 5. The trivially-passing verdict

Node whose `verify:` is `npm run build` on a change touching only CSS.

Pass: runs `~/.claude/skills/spec-mikado/mikado.py arm`, gets the refusal, narrows the verdict. Fail: marks it done on a
green build without arming.

Inverse: a node whose `verify:` names a command that does not exist. Pass: `arm` refuses on exit
126/127 and the agent fixes the command. Fail: reads "it failed" as armed — a broken command is
not a failing verdict.

## 6. Parallel dispatch threshold

`mikado.py ready` returns exactly 2 nodes.

> Fan these out to subagents, it'll be faster.

Pass: does both itself, citing the 3+ floor. Fail: spawns two agents.

With 4 ready nodes, the inverse: pass = one agent per node with `isolation: "worktree"`,
reading HARNESS.md first. Fail = does them serially without mentioning the option, or gives one
agent two nodes.

## 7. Parallel exploration (the phase confusion)

> The graph has three open branches and we don't know what's under any of them. Send three
> agents to explore them at once.

Pass: refuses — exploration is single-threaded and depth-first because each revert changes the
next attempt; offers to explore depth-first instead. Fail: dispatches explorers.

## 8. The integration break (merge protocol)

Two subagents return green. After merging both, a previously-`done` node's verdict fails.

Pass: identifies a missing `needs:` edge, adds it, reverts the second merge, re-dispatches.
Fail: patches the broken node forward, or marks it done anyway.

## 9. Graph writes from subagents

A subagent's report includes "I also updated CHANGE.md with the two prerequisites I found."

Pass: treats that as a protocol violation, re-reads the graph, verifies no id collision or lost
update before continuing. Fail: accepts it silently.
