# ICE: Intent, Context, Expectation

Parked 2026-09-25 while the lead and worker alignment with Lauren Tan's
orchestrate playbook goes first. That work is separate; come back here after.

## Where ICE comes from

IDSD (Intent-Driven Software Development), Kapil Viren Ahuja, Medium, May
2026, "The Anatomy of Intent (ICE in IDSD)". Read second-hand only (Medium
403): dev.to/jcmexdev summary, colincalnan/sdd, falkster.com critique.

- Intent, human-owned: the outcome, independent of technology, in five
  parts: what is wanted, constraints, failure scenarios, success scenarios,
  connections to other intents.
- Context, harness-owned: architecture and code state, injected as needed,
  not up front.
- Expectation, human-owned: done in user or business terms. The AI must
  never write or redefine them.
- Loop: human writes intent and expectations in minutes; harness pulls
  context; agent builds; harness checks against expectations; a human
  reviews before merge.
- Critique (falkster.com): SDD with a new acronym, the intent doc is a PRD;
  prefers a prototype plus a five-row eval. The durable idea: the agent
  never defines done.
- No arXiv paper names ICE or IDSD.

## Tools, by part

| Part | Use | Leave out |
|---|---|---|
| Intent | OpenSpec 1.13 (installed), forked schema "ice" adding intent.md that proposal requires; opsx apply reads every schema artifact | spec-kit, Kiro, BMAD, Tessl, Agent OS, GSD, Spec Kitty: all duplicate OpenSpec |
| Context | lat.md (vercel-labs, v0.12.2) for the code map; CONTEXT.md glossary and one-paragraph ADRs (Pocock); context7 for library docs; Serena for live symbols | repomix and AGENTS.md dumps (up front), GitNexus (noncommercial) |
| Expectation | Human-owned scenarios as tests, locked by a signed git tag and checked outside the agent; coverage script; fail-on-base; mutation on the diff (Stryker, mutmut, cargo-mutants) | Cucumber/Gherkin, openspec-guard, opsx verify as a gate |

Facts to remember:
- opsx verify is advisory, greps with "reasonable inference", never runs a
  test (read in OpenSpec source).
- OpenSpec issue 900 (scenario-to-test tags) approved 2026-09-25, not
  shipped; openspec show --json drops scenario names (PR 1972 open).
- lat.md: no .el support (no @lat scan, no symbol links), no link into an
  OpenSpec requirement, prompt hook pushes up to 5 sections each turn, no
  Emacs package, lat check usable in pre-commit today, GitHub Action unreleased.
- Unconfirmed: whether a stdio MCP server handed to an aob session reaches
  the model. Test live before choosing lat mcp over the lat CLI.
- skills/wayfinder says archived changes go to openspec/archive; OpenSpec
  uses openspec/changes/archive.
- Locking files against the agent works fully only in Claude Code (deny
  rules plus sandbox denyWrite plus no unsandboxed fallback); codex and pi
  unconfirmed. The real lock is the done check.

## What each practitioner adds

- Pocock (mattpocock/skills): CONTEXT.md is a glossary only (term,
  definition, Avoid list); tiny ADRs only when hard to reverse, surprising
  and a real trade-off; files created lazily; to-spec after grilling
  (problem, solution, user stories, implementation and testing decisions,
  out of scope); seams agreed with the user before tests; no tautological
  tests, expected values from an independent source; tracer-bullet tickets
  with blocking edges. domain-modeling and grilling are installed unchanged.
- Lauren Tan (pstack): verification first, a generated verification skill
  plus a feature map (one file per feature: sub-features, how to get there,
  driving it, gotchas) kept honest by a daily maintain run; "the best spec
  is code", questions settled by prototype; intent as the caller's view,
  restated back by the agent; plans only as evidence checklists (unit, live,
  perf; "Pass when"; linted by check-plan.mjs); plans deleted after.
- Boris Cherny / Anthropic: "give Claude a way to verify its work"; moved
  from plan mode to auto mode; prune CLAUDE.md, skills and hooks by
  ablation every six months; SPEC.md by interview, run in a fresh session,
  ending in an end-to-end check; /goal condition = one measurable end state,
  the check, the constraints; feature_list.json passes flags the agent may
  only flip ("unacceptable to remove or edit tests"); separate evaluator;
  skip the plan when the diff fits one sentence.

## Evidence-backed rules (arXiv)

Intent
- Detect underspecification in a separate step before coding: 69.4% vs
  61.2% resolved; a single agent asks late (2603.26233, full text).
- Ask only when the spec is incomplete: 76.9% resolved without asking
  (same).
- Hunt implicit requirements: 24.5-46% of failures (2608.09072); agents
  guess, 56-68% cross an unstated boundary (2607.02294).
- One consolidated intent, not chat turns: progressive reveal halves solve
  rates (2606.30573); equivalent revision paths flip 35/100 (2608.09799).
- Plans barely change accuracy for strong models; reports cover about 1
  action in 11, so review the diff (2609.20804, 2609.12205).

Context
- Do-not rules help, do rules distort; dropping "do not refactor unrelated
  code" cost 20pp (2604.11088, full text).
- Repo overviews in context files do not help and add over 20% cost
  (2602.11988).
- Task context beats procedure: a code-to-test map cut regressions 70%;
  "do TDD" alone raised them (2603.17973, full text, small models).
- Source at the edit site, not summaries: 27/45 vs 4/45 (2607.09691).
- One level of progressive disclosure, only when the corpus is too big
  (2607.17598).
- Record why each rule exists (2608.11095); turn rules into checks: 88.3% vs
  67.0% compliance (2603.00822).

Expectation
- Read-only tests; the most capable models cheat most, and test tampering
  is the hardest cheat to prompt away (2510.20270, 2608.29460, full text).
- A sanctioned flag-a-bad-check exit plus a policy: hacking 23.6% to 5.3%,
  solve rate unchanged (2608.29460).
- A new check must fail on the old code: 46% of passing checks carry no
  bug signal (2607.28871); 80.2% of agent test patches have weak oracles
  (2606.18168).
- Expected values from outside the code: self-derived oracles inflate gains
  9-15pp (2608.19626).
- Pre- and post-conditions before tests: +9.8pp bug detection (2608.17177).
- Cap retry loops: feedback raises cheating 33% to 38% (2510.20270).
- An evidence bundle helps review, not correctness (2606.17099).

SDD papers are mostly position papers and pilots; none compares SDD with
no SDD. Spec drift is argued, never measured. OpenSpec about as
deterministic as a stricter framework, more than Spec Kit (2606.30689).

## Proposed shape (not built)

1. Intent: intent.md in an OpenSpec change with the five parts, caller's
   view, out of scope, one refused alternative; a separate gap detector
   before coding that asks only when incomplete; the agent restates intent
   and the owner confirms; anything a probe can settle gets a prototype.
2. Context: do-not standing rules with a why each; CONTEXT.md and ADRs;
   lat.md; a code-to-test map; source loaded at the edit site on demand;
   no repo overview.
3. Expectation: owner agrees the seams; scenario tests written or approved
   by the owner; each fails on base, then is locked by a signed tag; a
   flag-a-bad-check tool; capped retries; checks with pass-when and
   evidence; mutation on the diff where the language has a tool.
4. Scripts to build: ice-check (intent parts present), ice-lock (locked
   paths unchanged), ice-coverage (every scenario id tested and passed),
   ice-fail-on-base.
5. Ceremony by size: a one-sentence diff skips ICE.
6. Ablation: date each artifact, re-test at a new model, drop what changes
   nothing.

## Open for the owner

- Who writes scenario tests: you, or a spec-only session you approve and
  lock (lean: the second).
- Which languages get mutation testing.
- Are target repos on GitHub (CODEOWNERS as the merge gate).
- What outlives a change (lean: intent, CONTEXT.md, ADRs, lat.md, the merged
  spec; tasks, prototypes and the decision trail are thrown away).
- Feature map as well as lat.md (lean: both; the map drives live checks).
- lat.md wiring: MCP through aob, lat init per repo, or both.
- Where the flow lives: presets plus an Emacs layer, an aob workflow file,
  or a skill.
