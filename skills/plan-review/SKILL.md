---
name: plan-review
description: Review a design or plan before code, in three read-only lenses (product/scope, UX, engineering); sort every finding fix, taste, or owner authority.
---

# Plan review

Before code is written against a non-trivial design, run three lenses against
the design doc. Each lens is a fresh, read-only worker with no shared context,
so it forms its own view instead of anchoring on the others.

1. Product and scope: references/product-scope.md
2. UX: references/ux.md
3. Engineering: references/engineering.md

## Sorting findings

Every finding gets exactly one tag:

- fix: the plan's author fixes it before anything else runs.
- taste: the owner picks; give options and a default.
- authority: the finding changes a premise or the scope; only the owner
  decides.

Two lenses agreeing on a finding is a signal to weigh, not a verdict to
auto-apply.

## Output

Write four files beside the design doc: one report per lens, plus a merged
list carrying every finding with its source lens and its tag.
