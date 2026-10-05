# Playbook: cleanup

Pick when: prune worktrees or simulators, or free disk. Deletes owner
state with no review to catch a slip, so every gate below holds.

1. df -h /. Worktrees from `git worktree list` or worktrunk `wt`,
   never hand-typed paths. Classify each: merged, dirty, ahead, age,
   size.
2. The class is advice, not permission. Check no live aob session or
   agent uses the path, and ask the owner for pinned work.
3. Dirty: show the wip diff and wait. Untracked scratch: name the
   files. Only clean, merged, unused proceeds.
4. Remove the confirmed set one at a time (e.g. in Emacs,
   ygg-git-worktree-remove), then `git worktree prune`. Branches
   survive.
5. Simulators and caches, only what the owner has not kept: xcrun
   simctl delete unavailable, old runtimes, DerivedData, package
   caches.
6. df -h /; re-list. Evidence: before and after.

Report: space reclaimed, what was removed, what was held back and why.

Ends on: a re-listed set and a disk delta.

Escalate to the owner on any dirty or in-use path.
