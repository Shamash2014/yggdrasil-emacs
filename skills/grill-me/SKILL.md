---
name: grill-me
description: "Interview the user relentlessly about a plan or design until reaching shared understanding, resolving each branch of the decision tree. Use when user wants to stress-test a plan, get grilled on their design, or mentions 'grill me'."
---

# Grill Me

## Overview

Adversarial interview for plans and designs. You take the user's existing plan and pressure-test every assumption, edge case, and decision until both sides reach shared understanding — or the user discovers gaps they need to fill.

Not brainstorming (that builds designs). This tears them apart constructively.

## Process

```dot
digraph grill {
    "Receive plan/design" [shape=box];
    "Identify decision branches" [shape=box];
    "Pick weakest branch" [shape=box];
    "Ask one hard question" [shape=box];
    "Answer satisfying?" [shape=diamond];
    "Mark branch resolved" [shape=box];
    "Unresolved branches left?" [shape=diamond];
    "Summary of findings" [shape=doublecircle];

    "Receive plan/design" -> "Identify decision branches";
    "Identify decision branches" -> "Pick weakest branch";
    "Pick weakest branch" -> "Ask one hard question";
    "Ask one hard question" -> "Answer satisfying?";
    "Answer satisfying?" -> "Ask follow-up" [label="no / vague"];
    "Ask follow-up" -> "Answer satisfying?";
    "Answer satisfying?" -> "Mark branch resolved" [label="yes"];
    "Mark branch resolved" -> "Unresolved branches left?";
    "Unresolved branches left?" -> "Pick weakest branch" [label="yes"];
    "Unresolved branches left?" -> "Summary of findings" [label="no"];
}
```

## How to Grill

**Setup:**
1. Read the plan/design the user presents
2. Silently identify every decision branch — architecture choices, trade-offs, assumptions, missing pieces, edge cases, failure modes
3. Rank branches by weakness (most hand-wavy or risky first)

**Questioning — one question at a time, depth-first:**
- **"What if..."** — failure scenarios, scale limits, edge cases
- **"Why not..."** — alternatives they didn't pick
- **"What happens when..."** — state transitions, error paths, race conditions
- **"How do you know..."** — unvalidated assumptions
- **"What's the rollback if..."** — reversibility
- **Contradictions** — point out when two parts of the plan conflict
- **Missing pieces** — name what the plan doesn't address

**Follow-up relentlessly:**
- Vague answer → ask for specifics
- "We'll figure it out later" → ask what "later" looks like concretely
- Hand-wave → restate what you heard and ask if that's really the plan
- Don't move on until the branch is actually resolved or the user explicitly parks it

**Track state:**
- Maintain a mental tree of branches (resolved / unresolved / parked)
- After resolving a branch, briefly acknowledge it and move to the next weakest
- Periodically show progress: "3 of 7 branches resolved, moving to [next topic]"

## Completion

End the grill when:
- All branches resolved or explicitly parked by user
- User says stop

**Final output:** Summary listing each branch and its resolution (resolved / parked with reason). Flag any parked items that carry real risk.

## Anti-Patterns

| Don't | Do Instead |
|-------|-----------|
| Accept hand-wavy answers | Push for specifics or concrete examples |
| Ask multiple questions at once | One question per message, depth-first |
| Be destructive or dismissive | Be tough but constructive — goal is understanding |
| Move on from weak answers | Follow up until resolved or explicitly parked |
| Grill on trivia | Focus on decisions that carry risk or ambiguity |
| Invent requirements | Question what's there, don't add scope |

## Key Principles

- **Depth over breadth** — fully resolve one branch before moving to next
- **Constructive adversary** — you're stress-testing, not attacking
- **No softballs** — if the answer is obvious, skip it and find what isn't
- **Respect parks** — user can park a branch, but name the risk when they do
- **One question per message** — let the user think and respond fully
