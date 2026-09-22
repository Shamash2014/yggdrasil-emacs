# Domain Adjustments — where the deterministic-property premise bends

Read this when Phase 1 detects a domain whose correctness is statistical, non-deterministic, or external. The metric must stay **deterministic and binary**; these domains reach that with the right *kind* of property plus, sometimes, a second gate or a frozen baseline. Apply the matching adjustment when defining properties in Phase 3. (UI is the worked example — see `ui-mode.md`.)

The recurring moves are always the same three:
- **(a) make it deterministic** — fix seeds, fake the boundary, freeze time;
- **(b) shift from exact-output to relational/threshold properties** when an exact answer isn't assertable;
- **(c) freeze a baseline as a golden** and gate against regression.

Never weaken to a non-binary gate to "make progress."

## Data / ML / analytics
Output is statistical and seed-dependent, so exact-output properties don't exist. Use **metamorphic properties** (row permutation doesn't change aggregates; feature scaling preserves ranking; `train` then `predict` on training rows beats a baseline) plus a **threshold gate** on a *frozen* held-out validation set (accuracy/MAE ≥ X). Fix all seeds. Acceptance = metamorphic properties green AND metric ≥ threshold on the golden dataset. The validation set is the golden — it only grows.

## External I/O / network / third-party APIs
Live services are non-deterministic, slow, and side-effecting; they never run inside the loop. The two ways to fake a boundary map onto the property/example split, and the mistake is treating recorded cassettes as coverage:

- **Synthetic generated responses = the property tier (the metric).** A response generator emits *any* contract-conformant payload — every status code, nulls, pagination edges, extra fields, malformed-but-schema-valid input. Property: "the client handles any contract-conformant response correctly, and degrades safely on any non-conformant one." This is the only universally-quantified part, so it is the real coverage.
- **Recorded-real cassettes = the example/golden tier (the pin).** A captured real interaction is one concrete point. It pins what a synthetic fake cannot: that the contract you *assumed* matched reality at record time, plus real-world quirks (header casing, encoding, undocumented fields, rate-limit shapes). One cassette = one frozen example, never a substitute for the generator.
- **The loop runs only on deterministic replay** (generators + cassettes) — never the network, never real creds.
- **Recorded-real is also the drift oracle, out of loop.** Periodically (or gated in CI with real creds) re-record; if a fresh recording no longer satisfies the contract the generator assumes, the provider drifted — a real failure to surface, and the contract/generator must update. Pure synthetic fakes can never catch this.
- **Hygiene:** scrub secrets/PII before a cassette is frozen; cassettes are committed goldens. Default: synthetic-generated is the metric, recorded-real cassettes are pinned examples + the drift oracle.

## Concurrency / distributed systems
Bugs live in interleavings that ordinary tests miss. This is where generated **interaction/operation sequences** shine: properties via **deterministic simulation**, linearizability/serializability checks, or generated schedules over a model. A failing schedule is a *real* discovered bug — freeze it as an example (with its seed/schedule) and never retry-mask it.

## Performance
"Fast enough" is a budget, not correctness. Keep it a **separate gate** from the correctness metric: property = "for any input in the generated size range, time/allocations ≤ budget", gated on **relative regression vs a frozen baseline** (golden), run on a stable host. Treat as advisory unless a dedicated perf environment exists — never let perf noise revert a correct implementation.

## Generative / LLM-backed features
Exact output is not assertable. Use **invariant properties only** (output is always valid JSON/schema; never leaks a secret; always refuses the disallowed class; length/format bounds hold) plus an llm-judge that is **advisory, never a gate**. Fix the model + temperature=0 + seed where the provider allows, to keep the invariants reproducible.

## Numerical / floating-point
Exact equality is the wrong oracle. Properties carry **tolerances**: oracle-within-epsilon, relative-error bounds, monotonicity, and known identities. Generators must include the nasty regions (zero, denormals, NaN/Inf, huge magnitudes).

## Multi-domain targets
If the target spans several domains (e.g. a UI that calls an ML-ranked API), layer the adjustments: each tier keeps its own kind of property, and the acceptance metric is the conjunction of all of them green.
