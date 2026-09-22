# Phases 1–2 — Discovery & Goal

## Phase 1 — Repo Discovery

Scan the repository to understand the stack and, critically, **how tests run**. **Use a workflow / parallel sub-agents** for this (where the provider supports it): fan out readers over distinct subsystems/areas and synthesize their findings into the summary below — fall back to inline scanning only when no such capability exists.

### Steps

1. List the top-level directory structure.
2. Read the files that reveal stack, conventions, and test tooling:
   - README, CONTRIBUTING, CHANGELOG
   - package.json, pyproject.toml, Cargo.toml, go.mod, mix.exs, Gemfile, pom.xml, build.gradle, Makefile, justfile
   - tsconfig.json, .eslintrc, tox.ini, setup.cfg, CI workflows
3. Sample 3–5 source files plus 2–3 existing test files to learn the conventions (naming, structure, assertion style, fixtures).
4. Identify:
   - **Language(s)** and **frameworks**, and which **domain** the target falls in (plain logic, or one of: ui, data-ml, external-io, concurrency, performance, generative, numerical).
   - **Test runner** — the exact command (`just test`, `pytest -q`, `cargo test`, `go test ./...`, `npm test`, `mix test`, etc.). Honor any project rule (e.g. a `justfile`, or `MIX_ENV=test` for Elixir).
   - **Structured test output** — how to get *per-test* pass/fail, not just the runner's exit code (a JSON/JUnit/TAP reporter, or running tests by name/tag): `pytest --json-report`, `cargo test --message-format=json`, `go test -json`, `jest --json`, `mix test --formatter`. The loop needs to know *which* properties and examples pass.
   - **Property-based test framework** + **how to pin its seed** (reproducible runs) and where it stores counterexamples:
     - Python → Hypothesis (`--hypothesis-seed`, `.hypothesis/`) · JS/TS → fast-check (`{ seed }`) · Rust → proptest (`PROPTEST_*`, `proptest-regressions/`) / quickcheck · Go → testing/quick or rapid (`-rapid.seed`) · Elixir → StreamData · Java/Kotlin → jqwik · Haskell → QuickCheck/Hedgehog · Scala → ScalaCheck
     - If none is installed, note that adding it is the first setup step.

### Output to the user

```
Repo:        [name]
Stack:       [languages, frameworks]
Domain:      [plain logic | ui | data-ml | external-io | concurrency | performance | generative | numerical]
Test cmd:    [exact command]   ·   Per-test output: [reporter/flag]
Property fw: [framework — installed | needs install]   ·   Seed flag: [how to pin]
Conventions: [where tests live, naming, assertion style]
```

## Phase 2 — Goal Selection

If the user already stated a software goal, **skip this phase** and go to Phase 3 with that goal.

Otherwise ask:

```
What should I build or fix? Describe the goal as behavior, not implementation.

  Goal:    ______________________________________________
  (The behavior/feature/bugfix. State it as observable input→output.)

  Surface: ______________________________________________
  (Which function / module / endpoint owns this behavior?)

  Bounds:  ______________________________________________
  (Constraints: invariants, performance limits, formats, conventions to honor.)

Examples:
  Goal:    parse RFC3339 durations    | dedup-merge sorted streams | retry with jitter+cap
  Surface: lib/duration.py::parse     | mergeSorted(a, b)          | http.retry middleware
  Bounds:  total order preserved      | no dup keys, stable        | <= maxDelay, idempotent
```

If the goal is a **bug**, ask for a reproduction (input + expected vs actual). That reproduction becomes the first example-based test.
