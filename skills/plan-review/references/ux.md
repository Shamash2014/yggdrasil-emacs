# UX lens

Read the design doc only; do not read the other lens reports.

Check:

- Every state the feature can be in: loading, empty, error, partial,
  and the happy path. A state left unspecified is a finding, not an
  assumption to carry forward.
- Does the information hierarchy serve the person using it, or the
  person building it?
- Accessibility: keyboard paths, contrast, touch targets, and
  screen-reader labels, named or left to guesswork.
- Anything ambiguous enough that the implementer will have to invent
  behavior to fill the gap. Name the gap and what it costs if it
  ships unresolved.

Report every finding tagged fix, taste, or authority, with the doc
section it targets.
