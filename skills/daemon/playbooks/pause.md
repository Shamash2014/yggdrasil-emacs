# Playbook: pause

Pick when: the owner says pause, going offline, or stop cleanly. Never
on "keep going" or "don't stop".

1. Stop at a safe boundary: finish the current step or back out of
   it; start nothing new; cancel nested workers. Evidence: aob
   sessions and worktrees listed, none mid-write.
2. Nothing irreversible: no push, no PR unless one was already out.
3. One `wip:` commit of uncommitted edits per worktree, a one-line
   note in the body if the tree is broken. Evidence: status clean.
4. A resume note in a file outside the repo: intent, progress and what
   is verified, state, next steps, key files, gotchas. Point at the
   ledger instead of copying it.

Report: where the loop stands, what is on disk, commits and tree
state, the first action on resume. A pause, not a final report.

Ends on: a clean tree, a `wip:` commit, a resume note.

Resume with playbooks/pickup.md.
