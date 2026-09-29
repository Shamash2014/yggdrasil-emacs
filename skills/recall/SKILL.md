---
name: recall
description: "Rebuild working context for a topic or a time period (yesterday, today, this week, since a date) from chats, git and shared records. Use on /recall."
disable-model-invocation: true
---

# Recall

**Before you start or resume work, rebuild what happened and hand back a tight brief of where things stand and what to do next.** Use for "recall my work on X", "what did I do yesterday", "catch me up on this week", "where did I leave off".

Keep it tight. The heavy reading fans out to parallel subagents on the Sonnet model (the sonnet alias; never a larger one for this grunt work). The main thread keeps only their findings and the brief.

## 1. Lock the scope

A recall has two axes. Pin both and state them back before searching.

- **Window.** Turn the words into dates in local time: "yesterday" is the previous calendar day; "today" since midnight; "this week" since Monday; "since Tuesday", "last 3 days", a date or a range as said. With no window and a topic, default to the last 7 days. With no window and no topic, default to the last working day. Never quietly shrink "all" to "recent".
- **Topic.** A feature, file, subsystem, bug or repo, or none. With no topic the recall is by time: everything in the window, grouped by project.
- **Where.** With a topic, the active workspace unless the owner names more. With only a window, every project touched in the window, since that is the question.

If the owner gave a full state capsule (paths, branch, the change), use it and skip the mining. One specific chat to resume is a pickup, not a recall.

## 2. Mine the records in the window

Fan out one subagent per source below (Sonnet), in parallel. Each selects by real modification time or date inside the window (find -newermt START ! -newermt END, or date directories), never by file name order; greps the topic first when there is one; reads only matching regions; skips the current chat and obvious noise (subagent, eval and test chats). Each returns one block per item: date, project, topic, the owner's goal, decisions, open threads, struggles and corrections, artifacts (commits, branches, PRs, tickets, changes), each with its source path or id.

- **Agent chats.** Claude Code transcripts under each config home: ~/.agents-conf/FAMILY/claude/projects/SLUG/*.jsonl and ~/.claude/projects/SLUG/*.jsonl (SLUG is the workspace path with each "/" turned into "-"). Codex sessions under ~/.codex/sessions/YYYY/MM/DD/. Each jsonl line is one message; its cwd field names the project.
- **Git.** For each project the chats name, plus the active one: git log --all --since START --until END with author, subject and branch; git reflog for the same window catches branch switches and resets; open branches and uncommitted work now.
- **Editor records.** The repo's .aob/ worklogs and task notes, .ice/ledger.tsv rows and openspec/changes touched in the window, lat.md sections changed.
- **Shared record.** When a topic names a feature, file, subsystem or bug, hand the question "what is the current state, what was tried and did not hold, what are users still reporting" to the why skill's source investigators, in the same window widened as needed. Skip this for pure time recall with no topic.

Two or fewer items in a source: read directly, no subagent.

## 3. Verify against live state

A transcript is history. Check each commit, branch, PR, ticket or change it names against git and gh now: merged, open, reverted, or gone.

## 4. Write the brief

- **Window.** The dates and projects covered, and any source that was empty or unreadable.
- **Timeline.** For a window longer than a day, one line per day: date, project, what moved. Skip for a single day.
- **Capsule.** At most 5 bullets: what the work is and where it stands.
- **Threads.** One line each with exactly one tag: [merged #N], [open PR #N], [in flight BRANCH], [verified, uncommitted], [reverted #N], [planned, not started].
- **Problems.** At most 5 recurring ones, including reverted fixes and symptoms still reported.
- **Next move.** The single most useful next action, concrete.

Cite chat findings by transcript path or session id and shared-record findings by their source (commit, PR, ticket, permalink). Cut detail before cutting threads. Sanitize private context before any public output.
