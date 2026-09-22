# Phase 3 — Gherkin + Atomic Decomposition (with property metric)

Translate confirmed intent + repo evidence into example-based requirements. Write Gherkin where each scenario has one distinct behavior, concrete preconditions, a triggering action, observable outcomes, and boundary/failure examples where behavior differs. Make every `Then` concrete enough to become exact assertions (name the value, transition, effect, error type/message, count, ordering, or absence) — no `Then it works`.

## Handoff = two files + property metric

Decompose into the smallest dependency-ordered tasks that can complete one loop. For each task, create the two-file handoff from `templates.md`:
- `<NN>-<slug>.feature` — exactly one `Feature`, one focused `Scenario`/`Scenario Outline` (this scenario is a **seed example** for the loop).
- `<NN>-<slug>.md` — the execution contract, **plus** the handoff's property metric and seed examples:
  - **PROPERTIES** — 1–4 universally-quantified invariants that are the acceptance metric (archetype + generator). For specialized domains take the right *kind* of property from `ui-mode.md` (frontend) or `domains.md` (data/ML, external I/O, concurrency, performance, generative/LLM, numerical). A property must be able to fail.
  - **SEED EXAMPLES** — the `.feature` scenario (E1) plus known edge/boundary cases and any bug reproduction.

State **exact executable assertions** (subject, framework-native matcher, concrete expected value, Red failure/diff). Reject vague assertions ("works", "is valid", truthiness) and vague properties (anything that merely restates the implementation or cannot fail).

## Atomicity

Decompose recursively until every handoff delivers **one independently observable behavioral delta**, completes one short loop, and has no internal step that yields useful verified progress alone. Treat multiple code paths, acceptance outcomes, state transitions, side effects, error policies, or migration steps as presumptive split points. Reject handoffs joined by `and`/`then`/`plus` unless inseparable. Prefer more small dependency-ordered handoffs; never create bookkeeping-only tasks.

Recursive split test (apply before presenting):
1. Can one subset of assertions fail while another passes? Split.
2. Can one branch/outcome/transition/effect/compatibility rule/migration step/failure policy be implemented and verified without the others? Split.
3. Touches unrelated ownership areas or production concerns? Split at the dependency point.
4. Would the Red failure contain >1 independent behavioral delta? Split.
5. Could an intermediate state be green, useful, safe to merge? Make it its own handoff.
6. Conjunctions in the title/outcome? Rewrite or split until it names one behavior and one outcome shape.

Add an **Atomicity Proof** per handoff naming the single behavioral delta and why no smaller useful loop exists. After drafting the DAG, do a second decomposition pass from fresh context and split any remaining broad node.

## Adapters + gate

Choose adapters by inspecting the provider's installed skills/tools (narrowest matching skill → native capability → `none`); record them in the handoff so another provider can substitute an equivalent capability. Each handoff embeds `Required execution discipline: autoresearch loop (property-metric red-green-refactor)`.

Present the evidence map, task DAG, and complete two-file-plus-property handoff preview as **one implementation plan**. **Gate: ask for approval and stop. Do not self-approve.** After approval, materialize handoffs under `docs/cog3/<plan-slug>/tasks/`.
