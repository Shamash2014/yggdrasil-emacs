# Playbook: ship

Pick when: a change is to be committed, pushed, or handed over as a
verified diff.

1. a diff check, quick level: git status and the ledger's files. Stage
   only those, by path, never a blanket add. Evidence: git status and
   the staged list.
2. The project's own checks (tests, lint, build, type check) on exactly
   the staged set, build level. Unstage what fails and report it.
   Evidence: each check's output.
3. Propose the commit text in the project's commit style (git log,
   create-pr conventions). Evidence: the text.
4. Commit only when the owner said commit this turn. Respect any repo
   commit hook or no-auto-commit rule; never bypass one or hand a
   worker the way around it. Evidence: the commit hash.
5. Push only when asked, and open the PR or MR with create-pr
   (gh or glab). Evidence: the pushed ref.
6. prove-it-works, build level: verify the result in a throwaway
   instance of the app, never the owner's live one. Name what needs the
   owner's restart or reinstall; never do it yourself.

Report: staged set, checks, commit text or hash, push state, what
needs the owner's restart.

Ends on: a verified diff handed over, committed or pushed only as the
owner said.

Escalate to the owner when a hook refuses or a check fails on a file
outside the ledger.
