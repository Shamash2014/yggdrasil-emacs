# Phase 3 — Define the Metric: Property-Based Tests

For the goal, write **3–6 properties** — universally-quantified invariants that must hold for all valid inputs. **All properties green is the acceptance metric.** Nothing else defines "done." If a specialized domain applies, take the property *kind* from its reference (`ui-mode.md`, `domains.md`) — metamorphic, threshold, contract, etc.

## What makes a good property

1. **Universal** — holds for *all* valid inputs, not one case. "for any list, sort(sort(xs)) == sort(xs)" not "sort([3,1,2]) == [1,2,3]".
2. **Falsifiable by generation** — a generator can produce inputs that would break a wrong implementation.
3. **Independent** — each property pins a different facet (correctness, ordering, idempotence, roundtrip, bounds, error behavior).
4. **Spec, not implementation** — express the contract, never restate the code. A property that mirrors the implementation tests nothing.

## Useful property archetypes (pick what fits)

- **Roundtrip** — `decode(encode(x)) == x`
- **Invariant** — output always satisfies P (sorted, non-empty, bounded, well-formed)
- **Oracle / model** — result matches a slow-but-obviously-correct reference
- **Idempotence** — `f(f(x)) == f(x)`
- **Metamorphic** — relation between `f(x)` and `f(transform(x))` (e.g. `f(x++y)` relates to `f(x)`,`f(y)`)
- **Commutativity / associativity / ordering preservation**
- **Error contract** — invalid input ⇒ the specified error, never a crash or silent wrong answer

## Seed the example suite

Alongside the properties, write **2–4 concrete example-based tests**: the canonical happy path, known edge cases (empty, single, boundary), and — for bugs — the exact reproduction. These are the loop's starting gradient and a fast, deterministic signal.

## Output to the user

```
Acceptance metric — property tests for [goal] (ALL must be green):

  P1. [property] — [archetype] — generator: [what it produces]
  P2. ...

Seed example tests:
  E1. [name] — [input → expected]
  E2. ...

These properties define "done". Adjust any, or good to go?
```

Wait for confirmation. Incorporate edits. The agreed properties are a contract — they do not change during the loop except to be *strengthened* (never weakened to pass).
