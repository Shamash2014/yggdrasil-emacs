# Owner holds and recovery

Every open question to the owner is a hold in the ledger: the
question, the options, the lead's default, the date, and what waits on
it. A hold clears only on the owner's answer; nothing that depends on
an open hold is sent while it stays open.

Recovery: when a session resumes, or /daemon runs in a session that
already has work, reconcile before acting. Read the ledger's holds,
the todo list, and git status and diffs of the checkout and its
worktrees. Report what reconciliation found before sending anything.
