---
name: explain-change
description: 'Explain, visualise or name-check a diff, commit range, branch or PR: before/after structure diagram and a glossary check of new names.'
---

# Explain Change

Read-only. Suggest renames; never apply them, and never commit, check out, stash or reset.

## 1. Pick the change

Detect from what the user gave:

| Input | Diff |
|---|---|
| PR number or URL | gh pr diff N, plus gh pr view N --json title,body,baseRefName,files |
| a..b or a...b | git diff a...b and git log a..b |
| one commit | git show SHA |
| branch name | git diff BASE...branch |
| "working tree", "uncommitted", "my changes" | git diff HEAD, plus untracked files from git status --short |
| nothing | current branch against its base |

Base: the open PR's baseRefName if gh pr view finds one; else the branch that git symbolic-ref refs/remotes/origin/HEAD names; else main, then master. If the current branch has no commits ahead of base but the tree is dirty, use the working tree. Say in one line which change you explained.

## 2. Gather context

- Read the whole diff, then enough of each touched file to know what the changed code belongs to and who calls it.
- Read CONTEXT-MAP.md or CONTEXT.md at the repo root (and the per-context CONTEXT.md files the change touches). Hold each term and its _Avoid_ list.
- Commit messages and the PR body are hints about intent, not proof.

## 3. Write the answer

Compact. In this order:

1. **Which change**: one line (e.g. "branch feature/x against main, 4 commits, 9 files").
2. **Why**: one sentence on the purpose, from the code first and messages second.
3. **Structure**: one Mermaid diagram of the part of the system the change touches, in glossary terms. Pick one:
   - one diagram with changed nodes marked (default; best when the shape mostly stays):
     classDef added, changed, removed, and class lines assigning them
   - two small diagrams, Before and After, when the shape itself moves (a module split, a flow reordered)
4. **Shapes** (only when the change alters types, tables, API or event payloads): diff blocks in the show-me style, leading space for context lines and plus or minus for changes. Show the whole target shape instead when most of it is new.
5. **Naming check**: the table below.
6. **Suggestions**: renames and missing glossary terms, as a short list, only if the check found any.

## Naming check

List every identifier, file, table, column, endpoint, CLI command, config key and event that the change adds or renames. Skip locals whose scope is a few lines. For each, compare with the glossary terms and their _Avoid_ lists:

| Name | Domain term | Verdict |
|---|---|---|
| createPurchase | Order | synonym of Order (Purchase is under _Avoid_) |
| OrderPlaced | Order | ok |
| ShipmentBatch | none | term missing from glossary |
| handleData | none | vague |

Verdicts, exactly one per row:

- **ok**: uses the glossary term, or is a general programming word that needs no term.
- **synonym of X**: names a glossary concept with another word, especially one under _Avoid_.
- **term missing from glossary**: a domain concept the glossary does not define yet.
- **vague**: says nothing about what it holds or does (data, info, manager, handler, util, helper, process, item, thing) or clashes with how the same concept is named elsewhere in the change or its neighbours.

No CONTEXT.md: say so in one line above the table, drop the Domain term column, still mark vague and inconsistent names (two words for one concept, one word for two concepts), and recommend the domain-modeling skill to start a glossary.

## Diagram rules

- Mermaid only; the user renders fences inline with TAB. Graphviz and PlantUML are not installed.
- Under about 15 nodes. Show the touched part and one hop of neighbours, not the whole system.
- Labels in glossary terms; ids short and simple; quote labels with spaces, parentheses or colons.
- Colours only for the change markers.

See references/example.md for a full worked answer.

## Checks before sending

- The diagram labels and the Why sentence use glossary terms, never _Avoid_ words.
- Every added or renamed name in the diff is in the naming table.
- Nothing was edited, staged or committed.
