# Cog2 Templates

## Shared-Understanding Gate

```markdown
## Shared Understanding

Outcome:
Actors and entry points:
In scope:
Non-goals:
Behavior and invariants:
Failure and edge-case policy:
Constraints:
Acceptance evidence:
Resolved decisions:
Parked decisions and risk:

Confirm or correct this understanding before repository exploration begins.
```

## Implementation-Plan Gate

```markdown
## Repository Evidence

Current flow:
Relevant files and ownership:
Existing patterns to reuse:
Constraints and contradictions:
Baseline verification:

## Task Order

1. `<NN>-<slug>` depends on `<task ids or none>`

## Task Handoffs

For every task, include one `.feature` preview and one `.md` preview using the templates below. Do not place multiple scenarios in one handoff.

## Parked Risks

- <decision>: <risk>

Approve or revise this plan before implementation begins.
```

## Focused Gherkin Handoff

Path: `docs/cog2/<plan-slug>/tasks/<NN>-<slug>.feature`

```gherkin
Feature: <one observable capability>

  Scenario: <one behavior and one outcome shape>
    Given <concrete precondition>
    When <one triggering action>
    Then <observable subject> equals <concrete value>
```

Use `Scenario Outline` only when all examples follow the same behavior, path, and outcome shape. Otherwise split them into separate handoffs.

## Markdown Task Handoff

Path: `docs/cog2/<plan-slug>/tasks/<NN>-<slug>.md`

```markdown
# <NN> <Task Name>

Gherkin: `./<NN>-<slug>.feature`
Required execution discipline: `test-first red-green-refactor`
TDD adapter: `<provider-native skill identifier>` | `native instructions`
Specialist adapter: `<provider-native skill identifier>` | `<available capability>` | `none`
Debug protocol: `required` | `not applicable`

## Outcome

<single observable behavior this task delivers>

## Atomicity Proof

Single behavioral delta:
Why its assertions cannot fail independently:
Why no branch, outcome, state transition, side effect, migration step, or failure policy can become a separate useful green handoff:
Conjunction check: <scenario and outcome contain no hidden `and`/`then`/`plus` work>
Second-pass decomposition result: <kept atomic | split into task ids>

## Dependencies

- <task id and required green evidence, or none>

## Repository Evidence

- <file, symbol, test, or command that grounds this task>

## Debug Evidence

Complete this section when `Debug protocol` is `required`; otherwise write `Not applicable`.

Reproduction command:
Reproduction rate and environment:
Fail path:
Knobs enumerated and tested:
Ranked hypotheses:
Accepted root cause:
Disproof attempted and result:

| Run | Change or probe | Observation | Ruled in or out |
|---|---|---|---|
| 1 | <input, knob, breakpoint, or tagged probe> | <result> | <hypothesis impact> |

## Expected Changes

- <files or ownership boundary expected to change>

## Exact Assertions

Use the repository's actual test framework syntax. Every assertion must identify a concrete subject and expected value.

| Subject | Assertion | Expected | Why this proves the scenario |
|---|---|---|---|
| <observable expression> | `expect(<actual>).toBe(<expected>)` | <literal, exact object, error, count, order, or absence> | <scenario outcome covered> |

Forbidden substitutes: bare truthiness, unspecified snapshots, "no exception", or vague prose when an exact value, type, call, state, count, ordering, or absence is observable.

## Red

Test: <first focused test to add>
Command: `<narrow test command>`
Expected failure: <specific missing behavior, not setup failure>
Expected assertion delta: <exact actual value versus exact expected value, or exact missing effect>

## Green

Minimal implementation: <smallest behavior change that can pass Red>

## Verification

- `<focused command>`
- `<relevant regression command>`

## Non-Goals

- <behavior explicitly excluded from this task>

## Completion Evidence

Status: pending
Red evidence:
Assertion evidence:
Green evidence:
Regression evidence:
Files changed:
Deviations:
Final reproduction result:
Temporary instrumentation removed:
```

## Portable Goal Ledger

Use this when no native goal or persistent task API exists. Present it before the first interview question, update it at each gate, and persist it with approved handoffs.

```markdown
## Goal Ledger

Objective: <user-requested outcome>
Status: active | complete | blocked
Current phase: convergence | exploration | planning | execution | complete
Completed handoffs: <ids or none>
Pending handoffs: <ids or unknown before planning>
Blockers: <items or none>
Last verified evidence: <command, approval, or result>
```

## Execution Report

```markdown
## Result

Implemented behaviors:
Completed handoffs:
Atomicity and decomposition evidence:
Red evidence:
Assertion evidence:
Green evidence:
Regression verification:
Debug evidence and root cause:
Falsification evidence:
Experiment ledger summary:
Simplification adapter:
Simplification scope and refinements:
Post-simplification verification:
Architecture review adapter:
Architecture context and ADRs read:
Deepening opportunities or none:
Files changed:
Deviations:
Parked risks:
Goal adapter and final status:
```
