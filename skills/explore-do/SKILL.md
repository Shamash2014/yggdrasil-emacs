---
name: explore-do
description: Explore codebase deeply, gather experience, then fix it
---

# Explore & Do

You are tasked with investigating and fixing an issue. You MUST explore deeply and build understanding before acting.

## Phase 1 — Explore & Gather Experience

Do NOT skip this phase. Spend most of your effort here. Before any fix:

1. **Read the relevant files end-to-end** — not just the function with the bug, but the entire module and its neighbors
2. **Trace callers and callees** — understand who calls this code, what it calls, and how data flows through
3. **Check recent changes** — use `git log` and `git blame` on relevant files to understand recent modifications and intent
4. **Find related code** — search for similar patterns elsewhere that show the correct approach or have the same issue
5. **Understand the runtime behavior** — inspect data shapes, state transitions, and side effects in the problem area
6. **Build a mental model** — before moving on, you should be able to explain how this part of the system works as a whole, not just the broken piece

## Phase 2 — Diagnose

Based on your gathered experience:

- State the root cause clearly (not just symptoms)
- Explain WHY the current code fails in terms of the system's design
- Identify ALL locations that need changes (not just the obvious one)
- Check if this is a recurring pattern that needs fixing in multiple places

## Phase 3 — Fix

Apply the fix:

- **Target the root cause** — do not patch symptoms
- **Fix all instances** — if the same bug exists elsewhere, fix those too
- **Follow existing patterns** — match the codebase style exactly
- **Minimal changes** — do not refactor or improve unrelated code

## Phase 4 — Verify

After fixing:

1. Run tests if available
2. Manually trace the fix through the code path to confirm correctness
3. Check for edge cases the original code missed
