---
name: structure
description: Use after design-decision has locked a concept — to translate the concept into an ordered, dependency-sequenced outline of code changes plus the complete affected-modules table. The human reviews the outline and can reorder, drop, add, or rewrite any step before any code is touched. Implementation only starts when the human explicitly locks the outline.
---

# Overview

Structure is **Phase 4** in a non-trivial coding task:

1. `grill-me` — pin intent
2. research — map the codebase
3. `design-decision` — pick a concept (final implementation + location)
4. **structure** — *this skill*. Translate the concept into an ordered, human-reviewable outline. Human steers each step. Implementation does not start until the outline is locked.

Input contract — must be on the table before activating:
- A chosen concept from `design-decision` with explicit Location + API
- Research output (named files with paths)
- Intent constraints (success criteria, refusals)

Output:
- Ordered task outline — each task is one atomic, dependency-sequenced, verifiable change
- Affected-modules table — every file the human will see in the eventual diff
- Explicit human-steering loop — render → human edits → re-render → lock

**No production code until the human says "go" / "lock".**

# Process

## 1. Verify input

- Concept names explicit Location + API → ok
- Research output names files by path → ok
- If anything is missing, return to the upstream skill — do not improvise

## 2. Explore — read the codebase before generating steps

Research output is a summary. You must verify it against actual files:
- Open every file listed in Research + the chosen concept's Location by path
- Read each file end-to-end; note its public API, naming conventions, error handling style, and existing patterns you will be extending
- Trace one caller → callee chain through each integration point to confirm data flow matches what your steps claim
- If a named research file doesn't exist or differs from the concept's assumptions, surface it explicitly (return to `design-decision`)

Do not generate the outline until Location + API are grounded in files you've actually read.

## 3. Generate the ordered outline

Each step is one **atomic, reviewable change** — small enough that the diff for that step is reviewable on its own, large enough to be useful.

Order by **dependency**: foundations first, integrations later, smoke-test last. If two steps could swap, list them in the order you'd merge if shipping incrementally.

Step format:

```
Step <n>: <imperative title>
  Target:    <file path>:<symbol or line range>
  Kind:      new file | extend | replace | delete | move
  What:      <2–3 lines: what gets added or changed in concrete terms>
  Sketch:    <high-level code shape — see "What goes in a sketch" below>
  Why:       <ties to a concept field — Location / API / Flow / Lifecycle>
  Verify:    <how this step is checked: compile, grep, run, manual smoke>
  Depends:   <step numbers this requires; "none" for the first step>
```

Every step gets `Verify:` — a step you can't verify in isolation is too big or too vague.

### What goes in a sketch

The sketch shows the **shape** of the change, not the implementation. Think "diff outline" rather than "diff".

**Include:**
- Function signatures with arg / return types
- Key control-flow lines (`if cache hit return X / else scan + return`)
- New lines being inserted into an existing region, with a 1-line anchor showing where they go
- Public-API surface changes (added / removed / renamed exports)
- Data shape declarations (table layout, schema changes)

**Exclude:**
- Function bodies beyond 3–5 lines of pseudo-real code
- Full module rewrites
- Anything you'd just paste at lock time
- Boilerplate (imports, `local M = {}`, `return M`)
- Error-handling details (mention "errors propagate" or "errors swallowed" but don't write the pcall)

**Rule of thumb:** if the human can copy your sketch and run it, it's too low-level — that's an ordinary plan. If the human can read your sketch and tell you "no, swap the cache check and the scan", or "add a third return value", or "this signature is wrong", it's the right level.

Example of right level (sketch — verifies shape):

```lua
-- Step 2: cache + load
local _cache = { mtime, root, map }
function M.load()
  -- cache hit (root match + mtime match) → return cache.map
  -- else scan(root); update cache; return map
end
local function scan(root)  -- reads <root>/*.md → { name = body }
```

Example of too low-level (ordinary plan, not a sketch):

```lua
function M.load()
  local root = snippets_root()
  local mtime = vim.fn.getftime(root)
  if mtime < 0 then
    _cache.root, _cache.mtime, _cache.map = root, -1, {}
    return _cache.map
  end
  ...  -- 20 more concrete lines
end
```

## 4. Generate the affected-modules table

Every file the human will see in the diff goes here, with its role and the step(s) that touch it:

```
File                                Role                Touched in step(s)
lua/acp/snippets.lua                new                 1, 4
lua/acp/mentions.lua                edit                2, 3
lua/acp/workbench.lua               read-only-import    (caller, not changed)
```

Roles: `new`, `edit`, `replace`, `delete`, `move`, `read-only-import` (file is referenced but unchanged — listed so the human sees the dependency surface).

The table must be **exhaustive**. If the human will see a file in `git status` after this work, it is in the table.

## 5. Render to the human

One message: outline + table + a one-line explicit steering prompt:

> Reorder, drop, add, or rewrite any step. Or reply `lock` to start implementation.

Do not start with "OK to proceed?" — that's a bundled yes/no.

## 6. Steering loop

The human can:

- **Reorder** — "do step 5 before step 2"
- **Drop** — "skip step 4"
- **Add** — "add a step that does X between 3 and 4"
- **Rewrite** — "step 2 should target Y instead of Z"
- **Lock** — "lock" / "go" / "looks good, proceed" → outline frozen, implementation starts

After **every** human edit, re-render the **full** outline + table reflecting the change. Never silently modify multiple steps from one instruction; if a rewrite ripples (e.g., dependency order changes), surface the ripple as additional rewrites the human must confirm.

If during the loop you discover a hidden **design** decision (something that affects Location / API / Flow / Lifecycle of the chosen concept, not just step ordering), **stop and return to `design-decision`**. Do not silently extend the design under the cover of "structure".

## 7. Lock and implement

After explicit lock:

- Implement step-by-step in outline order
- After each step: run its `Verify:` and report the result before moving to the next step
- A step reveals a structural change is needed → **return to step 4** (re-render outline) — never drift mid-implementation

# Rationalizations to reject

| Excuse | Reality |
|--------|---------|
| "I'll just start coding from the locked concept" | The concept names *what* + *where*. Structure names *order* + *steps* + *verification*. Skipping it means making 5–10 silent ordering and decomposition choices. |
| "Step ordering is obvious" | If you can name an alternative ordering, it isn't. Show the order and the dependency chain. |
| "I'll write the full implementation in the sketch" | Sketches show shape; ordinary plans show implementation. If the human can copy-paste your sketch and run it, you've already locked the structure silently. Cap each sketch at 3–5 lines of pseudo-real code + signatures. |
| "Skipping the sketch — the human will see it at lock time anyway" | The sketch is what the human steers on. "Step 2 looks wrong" is only actionable if Step 2 has a visible shape. Required field. |
| "I'll re-render only the changed step after a rewrite" | The human is verifying the *whole shape* still holds after their edit. Render the full outline. |
| "Affected-modules list is overkill" | The human is approving a diff scope. Without the full file list they can't see what they're approving. |
| "User said 'looks good' to a partial outline" | Partial = unconfirmed. Render in full first, then accept lock. |
| "I'll surface the design issue I just found inside this outline" | Design issues belong in `design-decision`. Surfacing them mid-structure dilutes both phases and slips a hidden concept change past confirmation. |
| "I can drift mid-implementation if a better approach appears" | Drift = unannounced restructure. Return to step 4 instead. |
| "OK to proceed?" is fine as the prompt | "OK to proceed" is a bundled yes/no — it bundles all 8 steps into one accept. Use the explicit reorder/drop/add/rewrite/lock prompt. |
| "The user will tell me if they want changes" | Make the affordance explicit. Without it, the human accepts the outline because it looks "official". |

# Red flags — STOP and back up

- Any step missing `Verify:`
- Any step missing `Depends:` (write `none` if no dependency, but write something)
- Affected-modules table missing a file the steps touch
- Sketch is missing on any step (required field)
- Sketch contains a complete implementation (>5 lines of real code, full bodies, error handling) — that's an ordinary plan, not a sketch
- Asked "OK to proceed?" instead of the explicit steering prompt
- Locked without an explicit human "lock" / "go" / equivalent
- Silently modified the outline after a human rewrite without re-rendering in full
- Found a Location / API / Flow / Lifecycle change and kept going — return to `design-decision`
- Started step 2 before reporting the verify result for step 1
