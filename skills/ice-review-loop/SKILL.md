---
name: ice-review-loop
description: 'ICE adversarial review gate: two isolated reviewers check a diff against do-not rules. Apply after an item passes its checks, before owner review.'
---

# ICE review loop

A checkpoint that passes its checks can still be poor code. Two reviewers
who never saw the builder's reasoning check the diff against the repo's
written rules; every finding is fixed, and the loop repeats until both
approve. The owner sees the item only after that.

**Why:** rules the reviewer can cite make review enforceable rather than
taste. "Do not" rules help and "do" rules distort: dropping one "do not
refactor unrelated code" line cost 20 points of resolved tasks (arXiv
2604.11088). A retry loop also raises cheating, 33% to 38% with feedback
(2510.20270), so the loop is capped and the cap goes to the owner.

## If you are a reviewer

You are read-only. You write nothing and run nothing; your reply is the
review, and a build worker saves it as it stands. Your brief holds three
things and you judge from those alone:

- the diff of this item, pasted, or a path to a saved patch;
- the rules: lat.md/rules.md, every R line with its why and scope;
- the intent paragraph: what this item is for, in the owner's words.

You never get the builder's reasoning, plan or report, and you do not ask
for them. Read the files the diff touches when a finding needs the code
around a hunk, never to learn what the builder meant.

Check each hunk against each rule whose scope covers its path. A rule
whose Check line names a script or test is enforced by it; skip that
rule. "Check: none" means you check it. Report only a
breach of a written rule. A concern no rule covers goes under Unruled at
the end, one line each, and never blocks.

Every finding is one line:

    - R3 src/cart/total.ts:42: why this line breaks R3, in one sentence

The rule id, the file and line in the new code, and why. A finding
without all three is not a finding. Do not suggest the fix; the fixer
reads the rule.

End on one verdict line, alone and exactly one of:

    Verdict: approve
    Verdict: blockers N

approve when no finding is left, blockers with the count otherwise.

Do not approve to end the loop, and do not raise a finding to look
thorough. Nothing a rule does not say is a blocker.

## Running the loop

Whoever runs ICE runs this after .ice/ice-verify CHANGE passes the item
and before the owner sees it.

1. Take the diff of the item, git diff against the change's base limited
   to the item's Files, and the intent paragraph: the item's line in
   tasks.md plus the What is wanted part of intent.md. A reviewer cannot
   run git, so the diff goes into the brief, pasted.
2. Send two reviewers at once, each with only the diff, the rules and
   the intent paragraph. Never paste the builder's report.
3. Hand both replies to one build worker, the fixer. It first saves them
   unchanged to CHANGE/reviews/code-N.md, N the slice number, under a
   heading for the round, "## Round R, reviewer A" and "## Round R,
   reviewer B", then fixes every finding, each fix inside the item's
   Files.
4. Re-run the affected tests: .ice/ice-verify CHANGE again. If a fix
   changed anything a user sees, re-run the UI gate (ice-ui-review).
5. Send both reviewers the new diff, fresh, as in step 2. Stop when both
   approve.

A fixer that believes a finding is wrong does not fix it and does not
argue with the reviewer: it writes "disputed: R3 file:line, why" in the
review file and the item goes to the owner.

A finding with the same rule id and the same kind of breach in two items
of one change is a recurring finding: it becomes a learnings line under
the ice-learnings skill.

## The cap

At most three rounds. When round three still has blockers, stop: the
item goes to the owner with the review file, the open findings and a
pick. Never start a fourth round, and never loosen a rule, delete one or
narrow its scope to get past it; rules are the owner's, like the checks.

## Two model families, and the limit

The two reviewers should come from different model families, so they do
not share blind spots: one Claude, one Codex (GPT). A Claude lead can send
only Claude subagents, so under a Claude lead the fallback is two
different Claude models: the review worker once on opus, and once more
with the model set to sonnet on the call. This is a limit, not an
equivalent: two Claude models share much of their training. When the
change is risky or the reviewers agree too easily, the owner asks for a
Codex second opinion from Emacs by opening a codex session on the saved
diff with this skill as its prompt; its reply is saved as a third
reviewer in the same file. A Codex lead sends codex workers and the
fallback runs the other way.

## Done

The item is reviewed when CHANGE/reviews/code-N.md holds every
round and its last round ends with both reviewers on "Verdict: approve",
or the item has gone to the owner past the cap. This prints 2:

    awk '/^## Round .*reviewer A$/{t=""} {t=t $0 "\n"} END{printf "%s", t}' \
      CHANGE/reviews/code-N.md | grep -c "^Verdict: approve$"

etc/ice/ice-check reviews CHANGE N gates the same file, and the ui
review beside it when ui-N.md exists.
