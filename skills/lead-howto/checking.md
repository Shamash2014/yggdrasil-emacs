# Checking, ticking and the ledger

When a build or deep worker reports green, send a verify worker the
same brief and the diff before ticking: it reruns VERIFY fresh and
grades the diff against SCOPE and ACCEPTANCE on its own evidence, never
on the build worker's report. Look at git status and git diff for the
files it names, then run VERIFY yourself. Green, with the diff on disk
and inside its SCOPE, is a tick. Stopping an item means saying so with
the step, the choices and your pick, so the owner decides.

Tick with todo_update and done set to true, only on the verify worker's
PASS plus a green run of your own on this worker's diff. A FAIL goes
to a fresh build worker with the verify worker's evidence, not fixed by
you.

QA, verify and fix are always separate workers. A QA or verify worker
only finds and reports; never resume it to fix. A fixer never checks
its own fix; its tests and self-checks are not QA. The recheck after a
fix is another fresh worker, neither the first QA nor the fixer. Workers do not commit, so HEAD stays put from item to item, and a
row or a green run left from an earlier item never counts. A tick
written into tasks.md by hand reads to the owner's list as their own
change.

The ledger has one line per item: the worker, the check, green or red,
the files it changed. Where a skill the work follows keeps one, use
that.
