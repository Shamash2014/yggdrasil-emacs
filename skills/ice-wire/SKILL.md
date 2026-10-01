---
name: ice-wire
description: "Wire a repo for ICE (Intent, Context, Expectation) in one step: lat.md for the agents in use, OpenSpec with the ice schema as the default, a C4 skeleton, the ice checks in pre-commit, and the skills the flow needs. Run it again at any time; a second run changes nothing."
disable-model-invocation: true
---

# ICE Wire

One script does the wiring, and you run it; you do not wire by hand.

    ~/emacs-31/etc/ice/ice-wire.sh [REPO]

**Why:** wiring by hand drifts from repo to repo, and a step done twice
by an agent is a step done two ways. The script is deterministic and
idempotent: every write checks first, so a second run reports every item
as already there and touches no file. When the script and this page
disagree, the script is right.

## Before you run it

- The repo is a git repo and the working tree is where you mean it to be.
- lat (mise, npm:lat.md), openspec, likec4 and python3 are on PATH.
- ICE_AGENTS names the agents to wire, default claude,codex,pi.

The script runs lat and openspec with CODEX_HOME and the XDG homes
pointed at a throwaway directory, so neither can touch the owner's
~/.codex or ~/.config. Never run either init by hand outside it: openspec
init, run without a terminal, deletes legacy prompts from the global
~/.codex.

## What it wires

- lat.md/ for each agent, through lat init: CLAUDE.md and AGENTS.md
  sections, lat hooks, MCP entries and the lat-md skill.
- OpenSpec for the same agents, the ice schema in openspec/schemas/ice,
  and ice as the default schema in openspec/config.yaml, so openspec new
  change needs no --schema flag.
- docs/arch/ (or doc/arch/ where the repo already uses doc/): a LikeC4
  skeleton with TODO titles and its generated README.md. An existing arch
  folder is never touched.
- .ice/: ice-check, ice-archive-to-lat, ice-c4-drift and ice-lat-drift,
  so hooks and CI work without this machine.
- The ice-checks and likec4-dsl skills, copied beside the skills OpenSpec
  and lat already copy in.
- lat.md/features.md, the feature map, and lat.md/architecture.md, one
  link per C4 view, both listed in the lat.md index. CONTEXT.md and
  docs/adr are linked once they exist.
- The check chain: likec4 validate, likec4 format check, ice-c4-drift,
  the C4 README regenerated and staged, lat check, ice-check repo. It goes
  into every place the repo already has, not the first one found:
  - .pre-commit-config.yaml gets a local repo with those hooks, unless
    repos is not its last key; then the chain is listed for the owner
    instead;
  - .husky/pre-commit gets the lines appended;
  - .github/workflows gets ice.yml, a CI job on push and pull request
    that runs the chain without the README step, plus ice-lat-drift
    (it needs the change's base, so it is never a per-commit hook);
  - lefthook is never edited; the chain is listed for the owner.
  Only when none of these exist does it write a local git pre-commit
  hook, so a repo with only a workflow gets no local hook and nothing
  runs before a commit. An existing git hook it did not write, or hooks
  outside the repo (core.hooksPath), are left alone and listed instead.

## What it never creates

CONTEXT.md and docs/adr come into being only when there is something to
put in them: CONTEXT.md with the first term the owner agrees
(domain-modeling), an ADR with the first decision that is hard to
reverse, surprising and a real trade-off. Rerun the script afterwards so
lat.md links them.

## The flow it sets up

Explore, intent, expectations, tasks, in that order; the ice schema
enforces the order through its artifact dependencies.

1. Explore first; settle by probe or prototype whatever running
   something can answer.
2. intent.md; restate it to the owner and wait for confirmation.
   ice-check intent passes before the proposal.
3. expectations.md, following the ice-checks skill: the contract, then
   Given / When / Then scenarios with ids. ice-check expect passes.
4. tasks.md, the minimal plan. ice-check plan passes, and it requires
   expect to pass.
5. Before the owner archives, .ice/ice-lat-drift CHANGE is clean. After
   openspec archive, run .ice/ice-archive-to-lat on the archived change
   folder; it files the change in lat.md under its feature and runs lat
   check. Where openspec/specs or lat.md disagree with the code, the code
   wins and they are corrected.

## Finish with the checklist

The script ends with two lists: what was wired, and what the owner must
still decide. Hand the second list to the owner as it stands. Do not
decide those items yourself.
