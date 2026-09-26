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

## Synthesis (2026-09-25, owner's mapping)

Tan's approach as the spine, with more rigour and minimal plans; Pocock's
language layer and test discipline added.

### lat.md: Context, the project's bank
One place the harness looks things up, on demand, never pasted whole:
- code map: sections linked to code, @lat backlinks;
- feature map (Tan): one section per user-visible feature with
  sub-features, how to get there, driving it, gotchas; kept honest by a
  daily maintain run that reports clean, changed or blocked;
- archived specs: each archived OpenSpec change leaves a short section
  (what changed, why, where) linked from the feature it touched;
- how, why and recall results worth keeping, filed as sections;
- CONTEXT.md glossary and docs/adr (Pocock) linked from the lat index;
  ADR only when hard to reverse, surprising and a real trade-off.
Gaps to handle: no .el scanning; no link into one OpenSpec requirement;
the prompt hook pushes up to 5 sections a turn; wiring (MCP through aob
vs lat init) still open.

### OpenSpec: Intent
The change folder holds the intent, reached through exploration:
- explore first (opsx explore); anything a probe or prototype can settle
  is settled that way, not asked (Tan);
- a separate gap detector before coding surfaces hidden requirements and
  asks only what is still open (arXiv 2603.26233); grill only on real
  gaps (Pocock);
- intent.md: IDSD's five parts, the caller's view first (Tan), out of
  scope, at least one refused alternative, terms checked against
  CONTEXT.md;
- restate-back gate: the agent restates the intent in its own words and
  nothing proceeds until the owner confirms;
- ice-check refuses an intent with a missing part.

### Expectation: Given / When / Then to checks
Each scenario in the change's spec deltas (GIVEN, WHEN, THEN) becomes a
check the building agent can run and never edit:
- seams agreed with the owner first, the highest seam, ideally one
  (Pocock); expected values from the scenario, never derived from the
  code;
- unit at the seam; live through the verification skill driving the real
  app (Tan); perf against a trunk baseline with the number that fails;
- each new check fails on the locked base, or now (--red) before
  implementation, then is locked (signed tag, checked outside the agent);
- mutation testing on the diff where the language has a tool;
- a flag-a-bad-check exit for the agent; capped retries; an evidence
  bundle with every verdict.

### Test quality, in markdown only (skills/ice-checks)
Adopted rules, each with its evidence (arXiv, 2024-2026 survey of 31 papers):
1. Contract before checks: pre/post-conditions and undefined inputs, one
   check per untested line (+9.8pp bug detection, 2608.17177).
2. Expected values from the spec; checks for possibly buggy code written
   by a fresh subagent that sees only the spec (+79% bug-catching tests,
   2607.22883; 25% vs 14% faults found, 2607.05139).
3. Contract from existing code describes the corrected behaviour after an
   audit for logic and robustness gaps (+18.9% bugs caught, 2607.22883).
4. Property checks where a law holds, never filtering out edge regions
   (+5 to +24pp for most models, 2605.15229).
5. A listed never-bend policy plus stop-and-report (23.6% to 9.7%, 5.3%
   with a report path, 2608.29460; prompt D and the abort flag, 2510.20270).
Did not help as text: generic TDD procedure, chain-of-thought with code in
view, spec beside the code, pass-all-tests framing, coverage goals, the
agent's own tests as validation, iterating on feedback. Text is weakest
against editing test files: keep the lock.

### Minimal plans, more rigour than Tan
- tasks.md is tracer-bullet vertical slices with blocking edges (Pocock),
  one fresh context each; wide refactors run expand, migrate, contract;
- each item: files, the scenario it satisfies, Pass when, evidence path;
- linted like Tan's check-plan (ice-check covers plan and intent);
- the lead runs items, ticks only after its own VERIFY, keeps the verdict
  per commit; tasks and prototypes are thrown away after, the intent,
  lat.md sections, glossary and ADRs outlive the change.
- phase boundaries between stages: continue, clear, handoff, subagent,
  compact, in that order (Pocock).

### Scripts to build
ice-check (intent parts and plan lint), ice-lock (locked paths
unchanged), ice-coverage (every scenario id tested and passed),
ice-fail-on-base; plus a lat step that files an archived change.

### Ceremony by size
A one-sentence diff skips ICE; unattended or multi-agent work gets all of
it.

## Built

Scripts, in etc/ice (ice-wire copies all but ice-runner into .ice/):
- ice-wire.sh: lat.md, OpenSpec with the ice schema, a C4 skeleton,
  pre-commit hooks, the skills below for claude, codex and pi, lat.md
  rules and learnings seeded, .ice/config filled from the test runner and
  a baseline run; a second run changes nothing (--rebaseline records the
  baseline again).
- ice-runner: finds the runner (pytest, vitest, jest, cargo, go,
  flutter, ERT, else justfile, Makefile or npm test), fills .ice/config
  when absent or the untouched template, and runs the suite once,
  isolated, into .ice/state/baseline.json.
- ice-check: intent (Confirmed hash), expect, plan (Checkpoints first,
  Approved hash, one checkpoint per slice), reviews CHANGE N, repo.
- ice-lock, ice-fail-on-base (--red), ice-coverage, ice-scenarios,
  ice-verify (one verdict line and a ledger row), ice-c4-drift.
- ice-archive-to-lat: files an archived change under its feature,
  deletes docs/prototypes/CHANGE/, moves its learnings into
  lat.md/learnings.md.

Skills: ice, ice-checks, ice-review-loop, ice-ui-review, ice-prototype,
ice-learnings, likec4-dsl, create-verification-skill.

Presets: lead (workers gaps, review and ui), gaps, maintain.

Keys, SPC a k: R confirm intent and A approve checkpoints (the owner's
alone), o changes, s lat search, c connections, b C4 preview. Every other
ICE command is M-x only.

Import: SPC p i imports a project and offers two extras, none picked by
default: skills (install and refresh the config's skills for every agent)
and ice (wire the project, or refresh a wired one's scripts, skills and
baseline). ygg-project-import-extras preselects them. M-x ygg-ice-wire
wires a project on its own.

## Open for the owner

- Who writes scenario tests: you, or a spec-only session you approve and
  lock (lean: the second).
- Which languages get mutation testing.
- Are target repos on GitHub (CODEOWNERS as the merge gate).
- What outlives a change (lean: intent, CONTEXT.md, ADRs, lat.md, the merged
  spec; tasks, prototypes and the decision trail are thrown away).
- lat.md wiring: MCP through aob, lat init per repo, or both.
- Where the flow lives: presets plus an Emacs layer, an aob workflow file,
  or a skill.
