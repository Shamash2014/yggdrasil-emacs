---
name: design-decision
description: Use after grilling has pinned the user's intent AND research has mapped the relevant codebase — to produce 2–3 concrete design concepts that answer the final question — what gets built, and where does it live? User picks one before any code is written. If grilling or research is missing, defer to those upstream steps first.
---

# Overview

Design-decision is **Phase 3** in a non-trivial coding task:

1. **Grill** — pin the user's intent (success criteria, hard constraints, refusals). Skill: `grill-me`.
2. **Research** — map the relevant codebase: existing patterns, fit constraints, integration points, named files the change will touch.
3. **Design** — *this skill*. Produce 2–3 concrete concepts that answer **final implementation** + **location**. User picks one.

Input contract — these must already be on the table:
- **Intent:** success criteria, hard constraints, refusals
- **Research:** named integration points, conventions to fit, files the change will touch (with paths)

Output: 2–3 concept blocks with explicit *what gets built* and *where it lives*. User picks one. **No production code until a concept is chosen.**

# Process

## 1. Verify the input contract

- Can you write down ≥3 success criteria, hard constraints, refusals from grilling? If not → return to grilling.
- Can you name ≥3 files or modules from research, by path? If not → return to research (or do a tight research pass inline if the prior research was light — but never pretend you've read what you haven't).

## 2. Explore missing ground before generating concepts

Even with research input, read the actual files that will be touched:
- Open every file listed in Research by path; note the module's public API shape and conventions
- Trace one caller → callee chain through each integration point to confirm data flow matches what you'll write into `API:` and `Flow:` fields
- If a named research file doesn't exist or its signature differs from expected, surface it explicitly

Do not generate concepts until Location + API are grounded in files you've actually read.

## 4. Generate 2–3 concepts

Each concept differs on a **load-bearing axis** — where the new code lives, what existing code it extends, what it depends on, who calls it, when it runs.

A concept is **one coherent shape**: location + API + data flow + lifecycle, all fitting together. Two concepts differing only in cosmetics (names, formatting) are the same concept.

**Always 2–3. Hard rule.** A single concept is a bundled yes/no — there's no "vs. what". If you find yourself writing one block, **you missed the load-bearing axis**. Force at least one alternative on each of these axes until you have 2–3 distinct concepts:

- A *different location* — same shape, different module/file
- A *different abstraction* — extend an existing module vs. add a new one vs. inline at call sites
- A *different owner* — who calls whom; pull vs. push; sync vs. async
- A *different fit with constraints* — drop a soft constraint, satisfy a hard one differently

Even if research strongly suggests one direction, render 2–3 to expose *what's being chosen*. Discarding alternatives is a Phase 5 (`Recommend`) decision, not a Phase 4 (`Generate`) decision.

## 5. Render each concept — **use the block format, not prose**

The format below IS the answer to "final implementation + location". Every concept gets one block. **No prose substitutes.** Prose lets you handwave the comparison; the block format forces parity across concepts.

```
Concept «short-name»
  Location:  new file <path>; touches <existing path:line or symbol>; depends on <existing module>
  API:       <2-line public surface — function signatures or commands the rest of the codebase will use>
  Flow:      <2-line data flow — input → processing → output>
  Lifecycle: <when it loads, what it caches, when it invalidates, who owns cleanup>
  Strength:  <what this is best at>
  Cost:      <what it gives up>
  Best when: <which intent or research constraint makes this win — quote the constraint>
```

Render all concepts in one message, in the same format, so the user sees the comparison side-by-side. If a field is "same as Concept A", say so explicitly — don't drop the field.

## 6. Recommend + confirm

- Recommend one concept, tied verbatim to a stated input constraint.
- **"Which concept?" is the only allowed bundled question** — the alternatives are visible.
- User rejects all → you missed a constraint or a research gap. Return to grilling or research, not to picking.

## 7. Decisions inside the chosen concept

Naming, surface-level tweaks, anything still ambiguous *inside* the chosen shape → ask **one at a time**.

## 8. Edit + verify

- Implement exactly the chosen concept at the named locations.
- A new load-bearing decision surfaces during implementation? Pause. If it changes the shape, return to grilling. If it's local, ask one question.
- Re-read the changed regions against the concept point-by-point. Compile-clean ≠ verified.

# Rationalizations to reject

| Excuse | Reality |
|--------|---------|
| "I'll just propose one design" | One = bundled yes/no. There's no "vs. what". Always 2–3. |
| "Concepts without Location are fine — we'll figure out files later" | "Where does it live" is half the design. Without paths, the user is approving a shape they can't verify against the codebase. |
| "I already know the codebase from context" | Context paragraphs and prior memories are not the codebase. You haven't read it until you opened a file. |
| "I'll match the project's existing style" (without a path) | Said before reading = a lie. Cite a file or drop the claim. |
| "Concepts take too long" | Each concept is 7 lines. Coding the wrong design takes hours. |
| "User said keep it simple, so one concept" | "Simple" is a property *of* the chosen concept, not a reason to skip alternatives. |
| "User is in a hurry, so I'll just pick" | Hurry asks for speed, not silent ambiguity. Comparing 3 concepts is faster than rebuilding the wrong one. |
| "A single default plan is fine if the user trusts me" | Trust assumes you generated alternatives before recommending. A single plan hides the comparison. |
| "I'll grill more decisions instead of generating concepts" | Grilling is over. Now compare designs. Don't loop back mid-Phase 4. |
| "Two concepts are close enough" | Close concepts mean you didn't find the load-bearing axis. Keep going. |
| "Research already pointed at the answer — only one viable design" | Research finds *constraints*; design picks *shapes*. Multiple shapes can satisfy the same constraints. Generate alternatives anyway, even trivial ones. They expose the axis you're choosing on. |
| "I'll write prose instead of the block format — it's clearer" | Prose lets you handwave the comparison. The block format forces parity. Use it. |
| "Listing 'what I'm not doing' is enough — same as showing alternatives" | A negative list is not a comparison. Show the rejected design as a concept block, then explain why it loses. |

# Red flags — STOP and back up

- Rendered prose instead of the concept block format
- Concept block missing `Location:` or `API:` — those are the answer
- Generated only one concept, or "one design + a 'what I'm not doing' list"
- Output starts with "Design Decision:" (singular) instead of two/three concept blocks
- Two of your concepts differ only in cosmetics
- Cannot quote the input constraint that makes the recommended concept win
- Used 0 tools and skipped research input — entered design without ground truth
- Recommendation reason isn't a verbatim quote of an intent or research constraint
- Started coding before the user named which concept
- Found yourself asking new grilling questions instead of generating concepts — return to `grill-me`
