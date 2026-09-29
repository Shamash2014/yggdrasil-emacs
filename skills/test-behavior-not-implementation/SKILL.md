---
name: test-behavior-not-implementation
description: Test what code does for its caller via its public surface, not how. Use on /test-behavior-not-implementation or whenever a test is written or changed.
---

# Test behaviour, not implementation

A test is a claim about what the caller gets. It reads the public
surface the way a caller does and asserts the outcome, never the steps.

## What a test asserts

The result for an input, the state a caller can observe afterwards, the
error a caller receives, the call that reaches a real boundary such as
the network or the disk. Not the private helper that was called, not
the order of internal calls, not the shape of a private structure.

## What a test does not do

It does not mock what the code under test owns: a private function, an
internal class, a collaborator in the same module. It mocks only at a
boundary the test cannot cross, and then it asserts what crosses the
boundary, not how. It does not reach into internals through
reflection, private accessors or test-only hooks.

## Naming and shape

The name is the claim in words, what holds and when. One claim per
test. Arrange the caller's situation, act through the surface, assert
the outcome. A test that would break under a refactor which keeps every
caller happy is testing implementation; rewrite it against the
behaviour or drop it.

## When a test is hard to write

That is a finding about the code, not the test: the surface is too
wide or the unit too entangled. Say so in the closing words rather than
forcing a test through the internals.

## Flake and isolation smells

Spot these in a test you write or touch, and fix them rather than
work around them:

- **shared mutable state** between tests — give each test its own
  fixture or instance.
- **order dependence** — a test that only passes after another ran
  first; make setup explicit instead.
- **clock, timezone or locale dependence** — inject or freeze the
  clock/locale rather than reading the system's.
- **real network instead of a stub** — stub at the boundary the test
  cannot cross.
- **sleeps or timeouts as synchronisation** — wait on the actual
  condition, not a duration.
- **unordered-result assertions** — sort or compare as a set before
  asserting.
- **unseeded randomness** — seed it, or assert the invariant instead
  of the value.
- **external service with no fallback** — the test should not fail
  when that service is merely slow or down.
- **missing negative-path tests** — a smell in what's absent: every
  behaviour with an error case or a rejection needs one covering it.
