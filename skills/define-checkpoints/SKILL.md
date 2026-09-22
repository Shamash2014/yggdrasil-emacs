---
name: define-checkpoints
description: "Apply when turning a spec, a plan or a screen-by-screen rewrite into work a harness can run. Break it into checkpoints that each carry their own proof — criteria that fail today, the gates that apply, the files in scope, the proof to leave — so every slice can be proved without asking anyone."
disable-model-invocation: true
---

# Define Checkpoints

Turn the work into a sequence of checkpoints, each small enough to finish
in one turn and complete enough to be proved without you in the room. A
checkpoint nobody can check is a promise, not a checkpoint.

**Why:** Work handed out in one piece comes back unprovable: a reviewer
reads a large diff and trusts it or does not. Work handed out as
checkpoints comes back with its own argument — this failed before, it
passes now, here is what it looks like, here is the file that shows it.
The sequence is what earns autonomy: each slice that proves itself is one
the next slice can stand on.

## What each checkpoint carries

Six things. A checkpoint missing any of them will come back needing a
conversation, which is the thing this is for avoiding.

**What it is for.** One line, in the words of the work and not of the
code. Not "edit the reducer" but "the cart total updates when a line is
removed."

**Its criteria.** Shell commands that fail in the checkout as it stands
and pass when the slice is done. Take them from the project — what CI
runs, what the justfile or the manifest names for test and lint — and
never invent a command the project does not have. Run each one before you
write it down: a criterion that already passes proves nothing, and a
criterion that cannot run is worse than none.

**Which gates apply.** Not every slice needs every gate. A slice that
changes no screen needs no visual comparison. A slice that changes nothing
observable needs no run of the app. Name the gates this one earns, and say
why for any you leave out.

**The files it may touch.** The scope, written as paths. This is what
keeps a slice small in practice rather than in intention, and what lets
the work be handed out without the rest of the tree being at risk.

**The proof it must leave.** What the slice writes down so somebody can
see it worked without rerunning it: the captured screen, the log of the
run, the output of the check. Name the files and where they go. A slice
whose proof is "the tests passed" leaves nothing to look at.

**The reference to compare against.** For anything visual: the old screen,
the design, or the previous checkpoint's capture. A comparison without a
reference is an opinion.

## Deriving, not inventing

Read the project before you write a single checkpoint. Its CI config, its
task runner, its test layout, its existing checks. The criteria you write
should already exist as commands somebody runs; your work is choosing
which ones prove this slice, not authoring new ones. Where the project has
no check for what the slice changes, say so — a slice whose only proof
would be a check you also have to write is two checkpoints, not one.

## Ordering

Order the checkpoints so the sequence reads as an argument. The canonical
shape is the failing check first and the fix on top: one slice shows the
problem is real, the next shows it resolved. Other honest orders are a
subtraction before a reshape, a baseline capture before a treatment, the
scaffold before the feature. Each slice must stand on its own; none may
depend on a later one.

## What you write

A checkpoint becomes a step. In a workflow file that is:

    [step NAME]
    prompt   = what it is for, one line
    criteria = the command that fails today and passes when it is done
    criteria = another, in the order they should run
    gate     = yes

with gate set where the slice writes to the repository. The scope, the
proof and the reference travel with the slice rather than in that file:
name them in the prompt, and name the folder the proof goes to.

**Pattern:**
- One slice, one outcome, one turn. If you cannot say what proves it in a
  sentence, it is two slices.
- Write the criteria by running them. Red today, green when done, both
  observed and not assumed.
- Choose gates per slice and justify the ones you leave out.
- Give every slice a scope, a proof and — where it is visual — a
  reference.
- Order so the sequence proves itself to somebody reading it later.

## Where it goes wrong

A checkpoint too large to finish in a turn comes back half-done and
unprovable. A criterion that already passes hides a slice that did
nothing. A gate applied to every slice regardless makes the owner the
bottleneck and teaches everyone to wave it through. A slice with no scope
grows until it is the whole job again. And a proof nobody can open is the
same as no proof.

The planning complement to **sequence-verifiable-units**, which is the
discipline this shape serves, and to **prove-it-works**, which keeps each
check real.
