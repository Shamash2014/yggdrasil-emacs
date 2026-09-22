---
name: reimplement
description: Explore codebase deeply, gather experience, then fix it
---

# Reimplement

You are tasked with reimplementing or fixing code based on deep codebase exploration. Follow this workflow strictly.

## Phase 1 — Deep Exploration

Before writing ANY code:

1. **Map the architecture** — read project structure, entry points, and module boundaries
2. **Trace data flow** — follow the code path related to the task through all layers
3. **Catalog patterns** — identify naming conventions, error handling patterns, state management approaches, and testing patterns used in the codebase
4. **Find prior art** — search for similar implementations, utilities, or helpers that already exist and MUST be reused
5. **Read tests** — understand expected behavior from existing test cases

Spend at least 60% of your effort on exploration before proposing changes.

## Phase 2 — Analysis

After exploration, document your findings:

- What patterns does the codebase use for this type of code?
- What existing utilities/helpers should be reused?
- What are the integration points and potential side effects?
- What conventions (naming, structure, error handling) must be followed?

## Phase 3 — Reimplementation

Now implement the fix or rewrite:

- **Match existing patterns exactly** — do not introduce new patterns when the codebase has established ones
- **Reuse existing code** — prefer calling existing functions over writing new ones
- **Preserve interfaces** — keep public APIs stable unless explicitly asked to change them
- **Minimal diff** — change only what is necessary, do not refactor surrounding code
- **No new dependencies** unless explicitly requested

## Phase 4 — Verification

After implementation:

1. Run existing tests to confirm nothing broke
2. Verify the fix addresses the original issue
3. Check that your code follows every pattern you cataloged in Phase 2
