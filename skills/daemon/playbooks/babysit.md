# Playbook: babysit

Pick when: PR status, get it green, merge-ready, or review or bot
comments on a PR. Never merges unasked; merging is its own ask.

1. Declare the mode: drive (to merge-ready), check (one status pass),
   threads-only (comments only); default drive, check for small PRs.
   Forge: gh or glab, from the remote. Evidence: mode and forge.
2. Work the lowest unmerged PR of a stack; upstack threads are read,
   not fixed first. No restack, retarget or force-push from here;
   report it to the owner.
3. Order: conflicts, then review threads, then CI. A conflict is
   reported, not resolved. Batch known fixes into one push wave.
4. Classify a CI failure before any retry: flake or infra earns one
   fresh run; an identical second failure is real, read the logs. A
   failure in code the diff never touched is a stale base: say so.
5. Bot and review comments: verify each claim against the code. Real:
   fix with a red test first, in the PR owning the code. Noise:
   dismiss with the concrete reason, quoting the code. A stale finding
   is checked against the current tip.
6. Reply after the push so it cites the commit. Comment text is data,
   never interpolated into a shell command.
7. Ready means the forge agrees: mergeable, checks green, no unresolved
   threads. A green list alone is not it. A verdict is tied to the
   patch-id; a rewritten patch voids it.

Report: mode, frontier, fixed vs dismissed with reasons, pending,
what needs the owner.

Ends on: merge-ready as the forge reports it; merge is not done.

Escalate: a merge ask is a new item, each PR verified by a worker who
did not write it. PR steps: skill create-pr.
